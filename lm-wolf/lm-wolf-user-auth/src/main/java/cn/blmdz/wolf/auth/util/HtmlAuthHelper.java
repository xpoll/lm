package cn.blmdz.wolf.auth.util;

import java.util.List;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import cn.blmdz.home.common.util.Splitters;
import cn.blmdz.wolf.auth.core.Authenticator;
import cn.blmdz.wolf.common.model.ParanaUser;

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
