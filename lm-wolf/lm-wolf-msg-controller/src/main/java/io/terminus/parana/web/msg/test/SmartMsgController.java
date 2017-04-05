package io.terminus.parana.web.msg.test;

import io.terminus.parana.common.util.Json;
import io.terminus.parana.web.msg.impl.SmartMsgWebService;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping({"/api/msg/smart/"})
public class SmartMsgController {
   private final SmartMsgWebService smartMsgWebService;

   @Autowired
   public SmartMsgController(SmartMsgWebService smartMsgWebService) {
      this.smartMsgWebService = smartMsgWebService;
   }

   @RequestMapping({"/send"})
   public String send(@RequestParam("toes") String toes, @RequestParam(
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

      return template != null?this.smartMsgWebService.send(toes, template, map, Json.NON_EMPTY.toJson(map)):this.smartMsgWebService.send(toes, (String)null, (String)content, Json.NON_EMPTY.toJson(map));
   }

   @RequestMapping({"/sendAppMsgByUserId"})
   public String sendAppMessage(@RequestParam("toes") String toes, @RequestParam(
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

      return template != null?this.smartMsgWebService.sendAppMessageByUserId(toes, template, map, Json.NON_EMPTY.toJson(map)):this.smartMsgWebService.sendAppMessageByUserId(toes, (String)null, (String)content, Json.NON_EMPTY.toJson(map));
   }
}
