package cn.blmdz.wolf.web.mock.pay.manager;

import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class AbstractMockGatewayManager {
   private static final Logger log = LoggerFactory.getLogger(AbstractMockGatewayManager.class);

   public abstract String serve(HttpServletRequest var1, HttpServletResponse var2);

   public abstract Map payConfirm(HttpServletRequest var1);
}
