package week1

/**
 * Extended Exercise 3 - Music
 * Not entirely sure what to do with this one - choosing to model Songs, Artists, Albums, Playlists, Genres in a pretty simple way that could be extensible
 * Limitations/Questions:
 * 1. Is my expression of genre good or is there an Enum or something that I should use instead?
 * 2. More fields for Song or Artist? Any fields for genre?
 * 3. Tried to make expandable to media beyond music (aka movie)
 */
sealed trait Genre
case object Pop extends Genre
case object Rock extends Genre
case object Jazz extends Genre
//more here...

sealed trait Artist
final case class SoloArtist(name: String) extends Artist
final case class Band(bandName: String, members: Seq[Artist]) extends Artist //wonder if this should be restricted to solo artist...
final case class Collaboration(members: Seq[Artist]) extends Artist

sealed trait Multimedia
final case class Song(name: String, artist: Artist, genre: Set[Genre], length: Double) extends Multimedia //genre a set because a song could fall into more than one
//could maybe model movies or other types of library items too, but limitng this to music

sealed trait MultimediaCollection
final case class PlayList(members: Seq[Multimedia]) extends MultimediaCollection
