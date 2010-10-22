/*
 * Copyright 2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb {
package common {

import _root_.org.specs._
import _root_.net.liftweb.common.Box._
import _root_.org.specs.runner._
import _root_.org.specs.Sugar._

class ConversionsSpecTest extends Runner(ConversionsSpec) with JUnit with Console
object ConversionsSpec extends Specification {
  "A StringFunc" should {
    "be created by a String constant" in {
      val sf: StringFunc = "Foo"

      sf.func() must_== "Foo"
    }


    "be created by a String Function" in {
      val sf: StringFunc = () => "Bar"

      sf.func() must_== "Bar"
    }

    "be created by a constant that can be converted to a String" in {
      implicit def intToString(in: Int): String = in.toString
      val sf: StringFunc = 55

      sf.func() must_== "55"
    }

    "be created by a function that can be converted to a String" in {
      implicit def intToString(in: Int): String = in.toString
      val sf: StringFunc = () => 55

      sf.func() must_== "55"
    }

  }
}

}
}
