package cn.blmdz.wolf.web.front.item;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.google.common.base.Objects;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.wolf.common.model.ParanaUser;
import cn.blmdz.wolf.item.model.Item;
import cn.blmdz.wolf.item.service.ItemReadService;
import cn.blmdz.wolf.item.service.ItemWriteService;
import cn.blmdz.wolf.web.core.util.RichTextCleaner;

@RestController
@RequestMapping({"/api/seller/items/{id}/detail"})
public class Richtexts {
   private static final Logger log = LoggerFactory.getLogger(Richtexts.class);
   private final ItemReadService itemReadService;
   private final ItemWriteService itemWriteService;

   @Autowired
   public Richtexts(ItemReadService itemReadService, ItemWriteService itemWriteService) {
      this.itemReadService = itemReadService;
      this.itemWriteService = itemWriteService;
   }

   @RequestMapping(
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   public Boolean editRichText(@PathVariable("id") Long itemId, @RequestParam("detail") String richText) {
      Response<Item> rItem = this.itemReadService.findById(itemId);
      if(!rItem.isSuccess()) {
         log.error("failed to find item(id={}), error code:{}", itemId, rItem.getError());
         throw new JsonResponseException(rItem.getError());
      } else {
         Item item = (Item)rItem.getResult();
         ParanaUser user = (ParanaUser)UserUtil.getCurrentUser();
         if(!Objects.equal(user.getShopId(), item.getShopId())) {
            log.error("item(id={}) not belong to user(id={}, shopId={})", new Object[]{itemId, user.getId(), user.getShopId()});
            throw new JsonResponseException(401, "item.not.owner");
         } else {
            String safeRichText = RichTextCleaner.safe(richText);
            Response<Boolean> r = this.itemWriteService.editRichText(itemId, safeRichText);
            if(!r.isSuccess()) {
               log.error("failed to edit richtext for item(id={}), error code:{}", itemId, r.getError());
               throw new JsonResponseException(r.getError());
            } else {
               return Boolean.valueOf(true);
            }
         }
      }
   }

   @RequestMapping(
      method = {RequestMethod.GET},
      produces = {"application/json"}
   )
   public String findRichTextById(@PathVariable("id") Long itemId) {
      Response<String> r = this.itemReadService.findRichTextById(itemId);
      if(!r.isSuccess()) {
         log.error("failed to find rich text detail for item(id={}), error code:{}", itemId, r.getError());
         throw new JsonResponseException(r.getError());
      } else {
         return (String)r.getResult();
      }
   }
}
