package cn.blmdz.hunt.engine.utils;

import java.net.URL;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.google.common.io.Resources;

@Component
public class ClasspathFileLoader implements FileLoader {
   private static final Logger log = LoggerFactory.getLogger(ClasspathFileLoader.class);

   public FileLoader.Resp load(String path) {
      path = Protocol.removeProtocol(path, Protocol.CLASSPATH);

      try {
         URL url = Resources.getResource(path);
         FileLoader.Resp resp = new FileLoader.Resp();
         resp.setContext(Resources.toByteArray(url));
         resp.setSign("UNSUPPORTED");
         return resp;
      } catch (Exception var4) {
         log.error("error when load classpath file: {}", path, var4);
         return FileLoader.Resp.NOT_FOUND;
      }
   }

   public FileLoader.Resp load(String path, String sign) {
      return FileLoader.Resp.NOT_MODIFIED;
   }
}
