package io.terminus.parana.web.front.item.search;

import io.terminus.common.model.Response;
import io.terminus.parana.search.item.ItemSearchReadService;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class Searches {
   private static final Logger log = LoggerFactory.getLogger(Searches.class);
   private final ItemSearchReadService itemSearchReadService;

   @Autowired
   public Searches(ItemSearchReadService itemSearchReadService) {
      this.itemSearchReadService = itemSearchReadService;
   }

   @RequestMapping(
      value = {"/api/search"},
      produces = {"application/json"}
   )
   public Response searchWithAggs(@RequestParam(
   required = false
) Integer pageNo, @RequestParam(
   required = false
) Integer pageSize, @RequestParam Map params) {
      String templateName = "search.mustache";
      return this.itemSearchReadService.searchWithAggs(pageNo, pageSize, templateName, params);
   }
}
