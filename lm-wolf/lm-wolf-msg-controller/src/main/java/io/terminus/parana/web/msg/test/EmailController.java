package io.terminus.parana.web.msg.test;

import io.terminus.parana.common.util.Json;
import io.terminus.parana.web.msg.MsgWebService;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
@RequestMapping({"/api/msg"})
public class EmailController {
   private final MsgWebService emailWebService;

   @Autowired
   public EmailController(@Qualifier("emailWebService") MsgWebService emailWebService) {
      this.emailWebService = emailWebService;
   }

   @RequestMapping({"/sendEmail"})
   @ResponseBody
   public String sendEmail(@RequestParam("toes") String toes, @RequestParam(
   value = "content",
   required = false
) String content, @RequestParam(
   value = "template",
   required = false
) String template, HttpServletRequest request) {
      Map<String, Serializable> map = new HashMap();

      for(String key : request.getParameterMap().keySet()) {
         map.put(key, request.getParameter(key));
      }

      return template != null?this.emailWebService.send(toes, template, map, Json.NON_EMPTY.toJson(map)):this.emailWebService.send(toes, (String)null, (String)content, Json.NON_EMPTY.toJson(map));
   }
}
