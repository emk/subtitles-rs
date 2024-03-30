//! A simple persistent LRU cache, implemented using `sqlite3`. This is not
//! built for speed, because it's caching expensive AI requests that are made
//! over the network. Instead, we focus on reliability.

use std::{marker::PhantomData, path::PathBuf};

use anyhow::Context as _;
use log::{debug, trace};
use rusqlite::Connection;
use serde::{de::DeserializeOwned, Serialize};

use crate::{Error, Result};

/// A simple persistent LRU cache, implemented using `sqlite3`.
///
/// We store the cache in `path`, in a table `substudy_cache` with the columns
/// `cache_name`, `key`, `value` and `last_used`.
pub(crate) struct Cache<K, V> {
    path: PathBuf,
    cache_name: String,
    approx_max_entries: u64,
    _phantom: PhantomData<(K, V)>,
}

impl<K, V> Cache<K, V>
where
    K: Serialize + DeserializeOwned,
    V: Serialize + DeserializeOwned,
{
    /// Make sure our cache database is set up.
    pub(crate) fn new(
        path: PathBuf,
        cache_name: &str,
        approx_max_entries: u64,
    ) -> Result<Self> {
        let conn = Connection::open(&path)?;
        conn.execute(
            "CREATE TABLE IF NOT EXISTS substudy_cache (
                cache_name TEXT NOT NULL,
                key BLOB NOT NULL,
                value BLOB NOT NULL,
                last_used TIMESTAMP DEFAULT(STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW')),
                CONSTRAINT cache_pk PRIMARY KEY (cache_name, key)
            )",
            [],
        )?;
        Ok(Self {
            path,
            cache_name: cache_name.to_owned(),
            approx_max_entries,
            _phantom: PhantomData,
        })
    }

    /// Get a value from the cache.
    pub(crate) fn get(&self, key: &K) -> Result<Option<V>> {
        // Don't check the cache if we're not supposed to cache anything.
        if self.approx_max_entries == 0 {
            return Ok(None);
        }

        let conn = Connection::open(&self.path)?;
        let key = bincode::serialize(key)?;
        let mut stmt = conn.prepare(
            "SELECT value FROM substudy_cache
            WHERE cache_name = ?1 AND key = ?2
            ORDER BY last_used DESC
            LIMIT 1",
        )?;
        let result = stmt
            .query_map((self.cache_name.as_str(), &key), |row| {
                Ok(bincode::deserialize(&row.get::<_, Vec<u8>>(0)?).unwrap())
            })?
            .next();
        match result {
            Some(Ok(value)) => Ok(Some(value)),
            Some(Err(e)) => Err(Error::new(e).context("could not read cache")),
            None => Ok(None),
        }
    }

    /// Put a value into the cache.
    pub(crate) fn put(&self, key: &K, value: &V) -> Result<()> {
        // Don't cache anything if we're not supposed to cache anything.
        if self.approx_max_entries == 0 {
            return Ok(());
        }

        let conn = Connection::open(&self.path)?;
        let key = bincode::serialize(key)?;
        let value = bincode::serialize(value)?;

        conn.execute("BEGIN", [])?;
        conn.execute(
            "INSERT OR REPLACE INTO substudy_cache (cache_name, key, value)
            VALUES (?1, ?2, ?3)",
            (self.cache_name.as_str(), &key, &value),
        )
        .context("could not write cache")?;

        // Count the number of entries in the cache.
        let count: u64 = conn
            .query_row(
                "SELECT COUNT(*) FROM substudy_cache WHERE cache_name = ?1",
                [self.cache_name.as_str()],
                |row| row.get(0),
            )
            .context("could not count cache entries")?;

        // If we have too many entries, delete the oldest ones. This is
        // ridiculously slow, so we amortize it.
        trace!(
            "cache count: {} (approx. max: {})",
            count,
            self.approx_max_entries
        );
        if count > self.approx_max_entries + self.approx_max_entries / 10 {
            debug!("cleaning cache");
            conn.execute(
                "DELETE FROM substudy_cache
                WHERE cache_name = ?1
                AND key NOT IN (
                    SELECT key FROM substudy_cache
                    WHERE cache_name = ?1
                    ORDER BY last_used DESC
                    LIMIT ?2
                )",
                (self.cache_name.as_str(), self.approx_max_entries),
            )?;
        }
        conn.execute("COMMIT", [])?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cache() {
        let _ = env_logger::builder().is_test(true).try_init();

        let tmp_dir = tempfile::tempdir().unwrap();
        let path = tmp_dir.path().join("cache.db");
        let cache = Cache::<String, String>::new(path.clone(), "test", 2).unwrap();
        let key1 = "key1".to_owned();
        let key2 = "key2".to_owned();
        let key3 = "key3".to_owned();
        let value1 = "value1".to_owned();
        let value2 = "value2".to_owned();
        let value3 = "value3".to_owned();
        cache.put(&key1, &value1).unwrap();
        cache.put(&key2, &value2).unwrap();
        assert_eq!(cache.get(&key1).unwrap(), Some(value1.clone()));
        assert_eq!(cache.get(&key2).unwrap(), Some(value2.clone()));
        assert_eq!(cache.get(&key3).unwrap(), None);
        cache.put(&key3, &value3).unwrap();
        assert_eq!(cache.get(&key1).unwrap(), None);
        assert_eq!(cache.get(&key3).unwrap(), Some(value3.clone()));
    }
}
