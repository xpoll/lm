package io.terminus.parana.web.front.item.search;

import io.terminus.common.model.Response;
import io.terminus.pampas.client.Export;
import io.terminus.parana.search.item.ItemSearchReadService;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MainSearch {
   private static final Logger log = LoggerFactory.getLogger(MainSearch.class);
   private final ItemSearchReadService itemSearchReadService;

   @Autowired
   public MainSearch(ItemSearchReadService itemSearchReadService) {
      this.itemSearchReadService = itemSearchReadService;
   }

   @Export(
      paramNames = {"pageNo", "pageSize", "params"}
   )
   public Response searchWithAggs(Integer pageNo, Integer pageSize, Map params) {
      String templateName = "search.mustache";
      return this.itemSearchReadService.searchWithAggs(pageNo, pageSize, templateName, params);
   }
}
