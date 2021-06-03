# Coterie
A coterie is a group of people with a common interest.

###### WIP

---

A home for live streamers and their communities.

## Features

#### Chat
- Custom username colors + gradients
- Create Roles for VIP's and Collectible Badges
- Unlimited emotes

#### Moderation
- Mod/Streamer View

#### Streamer Tools
- Admin View
- Stream statistics
- Promote other creators with the Shout Out Box

#### Behind The Scenes
- Fast. Very Fast. HASKELL AND ELM FAST!!!
- Back End = Haskell, Yesod, Lots of STM...
- Front End = Elm, Elm UI, Chroma.js...

---

## Use

Streamer specific parts of site instances are defined in
  - `src/Streamer.hs` using `src/Internal/StreamerInfo.hs`
  - `elm/src/Streamer.elm` using `elm/src/Internal/StreamerInfo.hs`
