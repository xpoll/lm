package io.terminus.parana.component.item.component;

import io.terminus.common.model.Response;
import io.terminus.pampas.client.Export;
import io.terminus.parana.component.dto.item.EditItem;
import io.terminus.parana.item.dto.FullItem;
import io.terminus.parana.item.service.ItemReadService;
import io.terminus.parana.rule.RuleEngine;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ItemReader {
   private static final Logger log = LoggerFactory.getLogger(ItemReader.class);
   private final ItemReadService itemReadService;
   private final RuleEngine ruleEngine;

   @Autowired
   public ItemReader(ItemReadService itemReadService, RuleEngine ruleEngine) {
      this.itemReadService = itemReadService;
      this.ruleEngine = ruleEngine;
   }

   @Export(
      paramNames = {"itemId"}
   )
   public Response findForEdit(Long itemId) {
      Response<FullItem> rFullItem = this.itemReadService.findFullInfoByItemId(itemId);
      if(!rFullItem.isSuccess()) {
         log.error("failed to find item(id={}), error code:{}", itemId, rFullItem.getError());
         return Response.fail(rFullItem.getError());
      } else {
         FullItem fullItem = (FullItem)rFullItem.getResult();
         EditItem editItem = new EditItem();
         editItem.setItem(fullItem.getItem());
         editItem.setItemDetail(fullItem.getItemDetail());
         this.ruleEngine.handleOutboundData(fullItem, editItem);
         return Response.ok(editItem);
      }
   }
}
