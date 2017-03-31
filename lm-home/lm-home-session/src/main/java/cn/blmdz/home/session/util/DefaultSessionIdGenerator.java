package cn.blmdz.home.session.util;

import com.google.common.base.Charsets;
import com.google.common.hash.Hashing;

import cn.blmdz.home.session.util.SessionIdGenerator;
import cn.blmdz.home.session.util.WebUtil;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;

public class DefaultSessionIdGenerator implements SessionIdGenerator {
   public static final Character SEP = Character.valueOf('Z');
   private final String hostIpMd5;

   public DefaultSessionIdGenerator() {
      String hostIp;
      try {
         hostIp = InetAddress.getLocalHost().getHostAddress();
      } catch (UnknownHostException var3) {
         hostIp = UUID.randomUUID().toString();
      }

      this.hostIpMd5 = Hashing.md5().hashString(hostIp, Charsets.UTF_8).toString().substring(0, 8);
   }

   public String generateId(HttpServletRequest request) {
      StringBuilder builder = new StringBuilder(30);
      String remoteIpMd5 = Hashing.md5().hashString(WebUtil.getClientIpAddr(request), Charsets.UTF_8).toString().substring(0, 8);
      builder.append(remoteIpMd5).append(SEP).append(this.hostIpMd5).append(SEP).append(Long.toHexString(System.currentTimeMillis())).append(SEP).append(UUID.randomUUID().toString().substring(0, 4));
      return builder.toString();
   }
}
