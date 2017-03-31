package cn.blmdz.home.zookeeper;

import java.net.InetAddress;
import java.util.UUID;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class HostNames {
   private static final Logger log = LoggerFactory.getLogger(HostNames.class);
   public static String hostName;

   static {
      String tempHostName = UUID.randomUUID().toString().substring(0, 6);

      try {
         tempHostName = InetAddress.getLocalHost().getHostName();
         log.info("get local host name:{}", tempHostName);
      } catch (Exception var2) {
         log.error("failed to get local host name, use uuid:{}", tempHostName, var2);
      }

      hostName = tempHostName;
   }
}
