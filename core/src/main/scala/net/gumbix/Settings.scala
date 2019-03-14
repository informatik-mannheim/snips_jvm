package net.gumbix

import java.util.Properties

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
object Settings {
  val properties = {
    val prop = new Properties();
    val in = getClass().getResourceAsStream("/Settings.properties");
    prop.load(in);
    in.close();
    prop
  }

  val rootDir = properties.getProperty("OUTPUTROOTDIR")
}
