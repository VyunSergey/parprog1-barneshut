package barneshut

import org.junit._

class BaseSuite {
  @Rule def individualTestTimeout = new org.junit.rules.Timeout(60 * 1000)
}
