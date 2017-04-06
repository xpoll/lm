package cn.blmdz.wolf.parana.component.item.component;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.wolf.parana.component.dto.item.EditItem;
import cn.blmdz.wolf.parana.item.dto.FullItem;
import cn.blmdz.wolf.parana.item.service.ItemReadService;
import cn.blmdz.wolf.parana.rule.RuleEngine;

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
