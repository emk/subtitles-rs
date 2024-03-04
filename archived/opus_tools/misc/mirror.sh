set -euo pipefail

base_url="http://opus.lingfil.uu.se/download.php?f=OpenSubtitles2016/"

for lang in $(cat langs.txt); do
    file="$lang.raw.tar.gz"
    url="$base_url/$file"
    if [ ! -e "$file" ]; then
        echo curl -Lo "$file" "$url"
        curl -Lo "$file" "$url"
    fi
done
