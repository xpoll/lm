package cn.blmdz.wolf.common.banner;

import java.io.PrintStream;
import org.springframework.boot.Banner;
import org.springframework.boot.ansi.AnsiColor;
import org.springframework.boot.ansi.AnsiOutput;
import org.springframework.boot.ansi.AnsiStyle;
import org.springframework.core.env.Environment;

public class ParanaBanner implements Banner {
   private static final String[] BANNER = new String[]{"", "  _____                                 \n |  __ \\                               \n | |__) |_ _ _ __ __ _ _ __   __ _      \n |  ___/ _` | \'__/ _` | \'_ \\ / _` |    \n | |  | (_| | | | (_| | | | | (_| |     \n |_|   \\__,_|_|  \\__,_|_| |_|\\__,_|  \n                                        \n                                   "};
   private static final String SPRING_BOOT = " :: Powered by Terminus.inc :: ";
   private static final int STRAP_LINE_SIZE = 42;

   public void printBanner(Environment environment, Class sourceClass, PrintStream printStream) {
      for(String line : BANNER) {
         printStream.println(line);
      }

      String version = "(v4.0)";

      String padding;
      for(padding = ""; padding.length() < 42 - (version.length() + " :: Powered by Terminus.inc :: ".length()); padding = padding + " ") {
         ;
      }

      printStream.println(AnsiOutput.toString(new Object[]{AnsiColor.GREEN, " :: Powered by Terminus.inc :: ", AnsiColor.DEFAULT, padding, AnsiStyle.FAINT, version}));
      printStream.println();
   }
}
