package cn.blmdz.hunt.engine.security;

import com.google.common.base.Charsets;
import com.google.common.hash.Hashing;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class CSRFUtil {
   private static final Logger log = LoggerFactory.getLogger(CSRFUtil.class);
   private static ThreadLocal threadToken = new ThreadLocal() {
      protected List initialValue() {
         return new ArrayList();
      }
   };
   private static final String SALT = "this is pampas CSRF token salt";
   private static final Random RANDOM = new Random();

   public static String genToken() {
      String token = Hashing.md5().hashString(RANDOM.nextInt() + String.valueOf(System.currentTimeMillis()) + "this is pampas CSRF token salt", Charsets.UTF_8).toString();
      ((List)threadToken.get()).add(token);
      return token;
   }

   public static List getAndClearThreadToken() {
      List<String> tokens = (List)threadToken.get();
      threadToken.remove();
      return tokens;
   }
}
