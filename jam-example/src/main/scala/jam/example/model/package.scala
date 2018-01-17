package jam.example

import java.sql.Timestamp
import java.time.Instant

import jam.data.Iso

package object model {

  implicit def isoEnumerator[A](implicit e: Enumerator[A]): Iso[A, String] =
    Iso.instance[A, String](e.to)(e.apply)

  implicit val isoInstantTimestamp: Iso[Instant, Timestamp] =
    Iso.instance[Instant, Timestamp](Timestamp.from)(ts =>
      Instant.ofEpochMilli(ts.getTime))

}
