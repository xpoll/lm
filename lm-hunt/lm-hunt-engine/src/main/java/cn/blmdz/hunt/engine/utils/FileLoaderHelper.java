package cn.blmdz.hunt.engine.utils;

import java.util.Map;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.collect.Maps;

@Component
public class FileLoaderHelper {
   @Autowired
   private HttpFileLoader httpFileLoader;
   @Autowired
   private LocalFileLoader localFileLoader;
   @Autowired
   private ClasspathFileLoader classpathFileLoader;
   @Autowired
   private ServletFileLoader servletFileLoader;
   @Autowired
   private RedisFileLoader redisFileLoader;
   private Map loaderMap = Maps.newHashMap();

   @PostConstruct
   private void init() {
      this.loaderMap.put(Protocol.HTTP, this.httpFileLoader);
      this.loaderMap.put(Protocol.NONE, this.localFileLoader);
      this.loaderMap.put(Protocol.FILE, this.localFileLoader);
      this.loaderMap.put(Protocol.CLASSPATH, this.classpathFileLoader);
      this.loaderMap.put(Protocol.SERVLET, this.servletFileLoader);
      this.loaderMap.put(Protocol.REDIS, this.redisFileLoader);
   }

   public FileLoader.Resp load(String path) {
      Protocol protocol = Protocol.analyze(path);
      FileLoader fileLoader = (FileLoader)this.loaderMap.get(protocol);
      if(fileLoader == null) {
         throw new UnsupportedOperationException("unsupported protocol: " + protocol);
      } else {
         return fileLoader.load(path);
      }
   }

   public FileLoader.Resp load(String path, String sign) {
      Protocol protocol = Protocol.analyze(path);
      FileLoader fileLoader = (FileLoader)this.loaderMap.get(protocol);
      if(fileLoader == null) {
         throw new UnsupportedOperationException("unsupported protocol: " + protocol);
      } else {
         return fileLoader.load(path, sign);
      }
   }
}
