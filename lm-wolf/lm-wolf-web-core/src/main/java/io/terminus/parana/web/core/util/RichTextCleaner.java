package io.terminus.parana.web.core.util;

import org.jsoup.Jsoup;
import org.jsoup.safety.Whitelist;

public class RichTextCleaner {
   public static final Whitelist whiteList = Whitelist.relaxed().addAttributes(":all", new String[]{"class"}).addAttributes(":all", new String[]{"style"});

   public static String safe(String richText) {
      return Jsoup.clean(richText, "", whiteList);
   }
}
