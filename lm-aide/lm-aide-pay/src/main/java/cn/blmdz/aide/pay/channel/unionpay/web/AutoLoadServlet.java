package io.terminus.lib.pay.channel.unionpay.web;

import io.terminus.lib.pay.channel.unionpay.sdk.SDKConfig;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;

public class AutoLoadServlet extends HttpServlet {
   public void init(ServletConfig config) throws ServletException {
      SDKConfig.getConfig().loadPropertiesFromSrc();
      super.init();
   }
}
