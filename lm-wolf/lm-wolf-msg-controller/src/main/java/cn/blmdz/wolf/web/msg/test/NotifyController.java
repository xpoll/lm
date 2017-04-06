package cn.blmdz.wolf.web.msg.test;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import cn.blmdz.wolf.common.util.Json;
import cn.blmdz.wolf.web.msg.MsgWebService;

@Controller
@RequestMapping({"/api/msg"})
public class NotifyController {
   private static final Logger log = LoggerFactory.getLogger(NotifyController.class);
   private final MsgWebService notifyWebService;

   @Autowired
   public NotifyController(@Qualifier("notifyWebService") MsgWebService notifyWebService) {
      this.notifyWebService = notifyWebService;
   }

   @RequestMapping({"/sendNotice"})
   @ResponseBody
   public String sendNotice(@RequestParam("toes") String toes, @RequestParam(
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

      return template != null?this.notifyWebService.send(toes, template, map, Json.NON_EMPTY.toJson(map)):this.notifyWebService.send(toes, (String)null, (String)content, Json.NON_EMPTY.toJson(map));
   }
}
