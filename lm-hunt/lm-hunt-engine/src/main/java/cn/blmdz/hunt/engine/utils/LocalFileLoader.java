package cn.blmdz.hunt.engine.utils;

import java.io.File;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.google.common.io.Files;

@Component
public class LocalFileLoader implements FileLoader {
   private static final Logger log = LoggerFactory.getLogger(LocalFileLoader.class);

   public FileLoader.Resp load(String path) {
      return this.load(path, (String)null);
   }

   public FileLoader.Resp load(String path, String sign) {
      if(path.startsWith(Protocol.FILE.getPrefix())) {
         path = Protocol.removeProtocol(path, Protocol.FILE);
      }

      File file = new File(path);
      if(!file.exists()) {
         return FileLoader.Resp.NOT_FOUND;
      } else if(sign != null && file.lastModified() == Long.valueOf(sign).longValue()) {
         return FileLoader.Resp.NOT_MODIFIED;
      } else {
         FileLoader.Resp resp = new FileLoader.Resp();
         resp.setSign(String.valueOf(file.lastModified()));

         try {
            resp.setContext(Files.toByteArray(file));
            return resp;
         } catch (Exception var6) {
            log.error("error when load local file: {}", path, var6);
            return FileLoader.Resp.NOT_FOUND;
         }
      }
   }
}
