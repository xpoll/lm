package io.terminus.parana.auth.util;

import io.terminus.common.utils.Splitters;
import io.terminus.parana.auth.core.Authenticator;
import io.terminus.parana.common.model.ParanaUser;
import java.util.List;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

public class HtmlAuthHelper {
   public static final String DATA_COMP_PATH = "data-comp-path";
   public static final String DATA_AUTH = "data-auth";

   public static String filter(Authenticator authenticator, ParanaUser user, String html) {
      Document document = Jsoup.parse(html);
      cleanHelper(document.body(), authenticator, user);
      return document.html();
   }

   private static void cleanHelper(Element element, Authenticator authenticator, ParanaUser user) {
      String dataAuth = element.attr("data-auth");
      List<String> keys = Splitters.SPACE.splitToList(dataAuth);
      boolean check = false;
      if(keys.isEmpty()) {
         check = true;
      } else {
         for(String key : keys) {
            if(authenticator.ask(user, key)) {
               check = true;
               break;
            }
         }
      }

      if(!check) {
         element.remove();
      } else {
         for(Element child : element.children()) {
            cleanHelper(child, authenticator, user);
         }

      }
   }
}
