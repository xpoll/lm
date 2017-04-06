package cn.blmdz.wolf.auth.util;

import com.google.common.io.Resources;

import cn.blmdz.wolf.auth.util.ParanaFileLoader;
import cn.blmdz.wolf.auth.util.Protocol;

import java.net.URL;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class ParanaClasspathFileLoader implements ParanaFileLoader {
   private static final Logger log = LoggerFactory.getLogger(ParanaClasspathFileLoader.class);

   public ParanaFileLoader.Resp load(String path) {
      path = Protocol.removeProtocol(path, Protocol.CLASSPATH);

      try {
         URL url = Resources.getResource(path);
         ParanaFileLoader.Resp resp = new ParanaFileLoader.Resp();
         resp.setContext(Resources.toByteArray(url));
         resp.setSign("UNSUPPORTED");
         return resp;
      } catch (Exception var4) {
         log.error("error when load classpath file: {}", path, var4);
         return ParanaFileLoader.Resp.NOT_FOUND;
      }
   }

   public ParanaFileLoader.Resp load(String path, String sign) {
      return ParanaFileLoader.Resp.NOT_MODIFIED;
   }
}
