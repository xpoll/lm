package cn.blmdz.wolf.parana.component.item.component;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.parana.cache.SpuCacher;
import cn.blmdz.wolf.parana.item.dto.FullItem;
import cn.blmdz.wolf.parana.item.model.Item;
import cn.blmdz.wolf.parana.item.model.ItemDetail;
import cn.blmdz.wolf.parana.item.service.ItemWriteService;
import cn.blmdz.wolf.parana.rule.RuleEngine;
import cn.blmdz.wolf.parana.rule.dto.BaseOutput;
import cn.blmdz.wolf.parana.rule.exception.InvalidException;
import cn.blmdz.wolf.parana.spu.dto.FullSpu;
import cn.blmdz.wolf.parana.spu.model.SpuDetail;

@Component
public class ItemWriter {
   private static final Logger log = LoggerFactory.getLogger(ItemWriter.class);
   private final ItemWriteService itemWriteService;
   private final RuleEngine ruleEngine;
   private final SpuCacher spuCacher;

   @Autowired
   public ItemWriter(ItemWriteService itemWriteService, RuleEngine ruleEngine, SpuCacher spuCacher) {
      this.itemWriteService = itemWriteService;
      this.ruleEngine = ruleEngine;
      this.spuCacher = spuCacher;
   }

   public Response create(FullItem fullItem) {
      Item item = fullItem.getItem();
      if(item.getSpuId() != null) {
         try {
            FullSpu spu = this.spuCacher.findFullSpuById(item.getSpuId());
            SpuDetail spuDetail = spu.getSpuDetail();
            item.setCategoryId(spu.getCategoryId());
            ItemDetail itemDetail = this.processItemDetailAgainstSpu(fullItem, spuDetail);
            fullItem.setItemDetail(itemDetail);
         } catch (Exception var6) {
            log.error("failed to find spu(id={}), cause:{}", item.getSpuId(), Throwables.getStackTraceAsString(var6));
            return Response.fail("spu.find.fail");
         }
      }

      return this.validate(fullItem)?this.itemWriteService.create(fullItem):Response.fail("item.create.fail");
   }

   private ItemDetail processItemDetailAgainstSpu(FullItem fullItem, SpuDetail spuDetail) {
      ItemDetail itemDetail = fullItem.getItemDetail();
      if(itemDetail == null) {
         itemDetail = new ItemDetail();
      }

      if(CollectionUtils.isEmpty(itemDetail.getImages())) {
         itemDetail.setImages(spuDetail.getImages());
      }

      if(!StringUtils.hasText(itemDetail.getDetail())) {
         itemDetail.setDetail(spuDetail.getDetail());
      }

      if(itemDetail.getPacking() == null) {
         itemDetail.setPacking(spuDetail.getPacking());
      }

      if(!StringUtils.hasText(itemDetail.getService())) {
         itemDetail.setService(spuDetail.getService());
      }

      return itemDetail;
   }

   public Response update(FullItem fullItem) {
      return this.validate(fullItem)?this.itemWriteService.update(fullItem):Response.fail("item.update.fail");
   }

   private boolean validate(FullItem fullItem) throws InvalidException {
      try {
         this.ruleEngine.handleInboundData(fullItem, (BaseOutput)null);
         return true;
      } catch (Exception var3) {
         log.error("failed to validate fullItem({}), cause:{}", fullItem.getItem(), Throwables.getStackTraceAsString(var3));
         Throwables.propagateIfInstanceOf(var3, InvalidException.class);
         return false;
      }
   }
}
