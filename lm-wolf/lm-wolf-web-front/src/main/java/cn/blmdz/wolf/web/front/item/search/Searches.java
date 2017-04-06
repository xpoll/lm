package cn.blmdz.wolf.web.front.item.search;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.parana.search.item.ItemSearchReadService;

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
