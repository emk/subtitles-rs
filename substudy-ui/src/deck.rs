//! A spaced repetition deck model, with the same basic datum/card distinction
//! as Anki.

use std::collections::HashMap;

use uuid::Uuid;

use crate::html::Html;

/// A deck of cards.
#[derive(Debug)]
pub struct Deck {
    notes: HashMap<Uuid, Note>,
    cards: HashMap<Uuid, Card>,
}

/// A card template.
pub struct Template {
    pub id: Uuid,
    pub name: String,
    pub front: String,
    pub back: String,
}

/// A "node", which may be used to generate one or more cards.
#[derive(Debug)]
pub struct Note {
    /// A unique identifier.
    pub id: Uuid,

    /// Fields displayed on the card.
    pub fields: HashMap<String, Html>,
}

/// A card, which is generated from a note.
#[derive(Debug)]
pub struct Card {
    /// A unique identifier.
    pub id: Uuid,

    /// Our underlying note.
    pub note_id: Uuid,

    /// The template used to generate this card.
    pub template_id: Uuid,

    /// Multiple cards may be generated from a single (note, template) pair. Which
    /// card is this?
    pub template_serial: usize,
}
