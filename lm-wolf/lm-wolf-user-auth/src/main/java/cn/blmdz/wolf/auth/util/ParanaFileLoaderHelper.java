package cn.blmdz.wolf.auth.util;

import com.google.common.collect.Maps;

import cn.blmdz.wolf.auth.util.ParanaClasspathFileLoader;
import cn.blmdz.wolf.auth.util.ParanaFileLoader;
import cn.blmdz.wolf.auth.util.ParanaHttpFileLoader;
import cn.blmdz.wolf.auth.util.ParanaLocalFileLoader;
import cn.blmdz.wolf.auth.util.ParanaServletFileLoader;
import cn.blmdz.wolf.auth.util.Protocol;

import java.util.Map;
import javax.annotation.PostConstruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ParanaFileLoaderHelper {
   @Autowired
   private ParanaHttpFileLoader httpFileLoader;
   @Autowired
   private ParanaLocalFileLoader localFileLoader;
   @Autowired
   private ParanaClasspathFileLoader classpathFileLoader;
   @Autowired
   private ParanaServletFileLoader servletFileLoader;
   private Map loaderMap = Maps.newHashMap();

   @PostConstruct
   private void init() {
      this.loaderMap.put(Protocol.HTTP, this.httpFileLoader);
      this.loaderMap.put(Protocol.NONE, this.localFileLoader);
      this.loaderMap.put(Protocol.FILE, this.localFileLoader);
      this.loaderMap.put(Protocol.CLASSPATH, this.classpathFileLoader);
      this.loaderMap.put(Protocol.SERVLET, this.servletFileLoader);
   }

   public ParanaFileLoader.Resp load(String path) {
      Protocol protocol = Protocol.analyze(path);
      ParanaFileLoader fileLoader = (ParanaFileLoader)this.loaderMap.get(protocol);
      if(fileLoader == null) {
         throw new UnsupportedOperationException("unsupported protocol: " + protocol);
      } else {
         return fileLoader.load(path);
      }
   }

   public ParanaFileLoader.Resp load(String path, String sign) {
      Protocol protocol = Protocol.analyze(path);
      ParanaFileLoader fileLoader = (ParanaFileLoader)this.loaderMap.get(protocol);
      if(fileLoader == null) {
         throw new UnsupportedOperationException("unsupported protocol: " + protocol);
      } else {
         return fileLoader.load(path, sign);
      }
   }
}
