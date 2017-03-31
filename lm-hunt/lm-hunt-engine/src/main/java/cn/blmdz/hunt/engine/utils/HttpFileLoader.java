package cn.blmdz.hunt.engine.utils;

import org.springframework.stereotype.Component;

import com.github.kevinsawicki.http.HttpRequest;

@Component
public class HttpFileLoader implements FileLoader {
   public FileLoader.Resp load(String path) {
      HttpRequest request = HttpRequest.get(path);
      if(request.notFound()) {
         return FileLoader.Resp.NOT_FOUND;
      } else {
         FileLoader.Resp resp = new FileLoader.Resp();
         resp.setContext(request.bytes());
         resp.setSign(String.valueOf(request.lastModified()));
         return resp;
      }
   }

   public FileLoader.Resp load(String path, String sign) {
      HttpRequest request = HttpRequest.get(path);
      request.header("If-Modified-Since", sign);
      if(request.notFound()) {
         return FileLoader.Resp.NOT_FOUND;
      } else if(request.notModified()) {
         return FileLoader.Resp.NOT_MODIFIED;
      } else {
         FileLoader.Resp resp = new FileLoader.Resp();
         resp.setContext(request.bytes());
         resp.setSign(String.valueOf(request.lastModified()));
         return resp;
      }
   }
}
