package jstengel.ezxml.extension


import scala.annotation.tailrec


object StringHelper {
    
    implicit class StringHelperClass(s: String) {
        def splitAt(matchString: String): (String, String) = {
            @tailrec def spanRec (toMatch : String, buffer : String): (String, String) =
                if(toMatch.startsWith(matchString) || toMatch.isEmpty)
                    (buffer, toMatch)
                else
                    spanRec(toMatch.tail, buffer + toMatch.head)
            spanRec(s, "")
        }
        def takeUntil(matchString: String): String = {
            splitAt(matchString)._1
        }
        def dropUntil(matchString: String): String = {
            splitAt(matchString)._2
        }
    }
    
    /**
     * used for matching the first two elements of a string
     */
    object ::: {
        def unapply(s: String): Option[(String, String)] =
            if(s.length < 3)
                None
            else {
                Some((s.take(2), s.drop(2)))
            }
    }
    
    /**
     * used for matching the first element of a string
     */
    object :: {
        def unapply(s: String): Option[(Char, String)] =
            if(s.length == 0)
                None
            else {
                Some((s.head, s.tail))
            }
    }
    
}
