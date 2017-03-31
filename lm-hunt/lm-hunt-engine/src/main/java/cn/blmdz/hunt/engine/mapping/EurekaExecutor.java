package cn.blmdz.hunt.engine.mapping;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.client.RestTemplate;

import cn.blmdz.hunt.engine.config.model.Service;

public class EurekaExecutor extends Executor {
   @Autowired
   private RestTemplate restTemplate;

   public boolean detectType(Service service) {
      return false;
   }

   public Object exec(Service service, Map params) {
      return null;
   }
}
