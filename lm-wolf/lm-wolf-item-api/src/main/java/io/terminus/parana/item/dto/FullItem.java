package io.terminus.parana.item.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.google.common.collect.Sets;
import io.terminus.parana.attribute.dto.SkuAttribute;
import io.terminus.parana.item.model.Item;
import io.terminus.parana.item.model.ItemDetail;
import io.terminus.parana.item.model.Sku;
import io.terminus.parana.rule.dto.BaseInput;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import org.apache.commons.collections.CollectionUtils;

public class FullItem extends BaseInput {
   private static final long serialVersionUID = -6794381566838883200L;
   private Item item;
   private ItemDetail itemDetail;
   private List skus;

   @JsonIgnore
   public Long getCategoryId() {
      return this.item != null?this.item.getCategoryId():null;
   }

   @JsonIgnore
   public Long getSpuId() {
      return this.item != null?this.item.getSpuId():null;
   }

   @JsonIgnore
   public Set getSkuAttrKeys() {
      if(CollectionUtils.isEmpty(this.skus)) {
         return Collections.emptySet();
      } else {
         Set<String> skuAttrKeys = Sets.newLinkedHashSet();

         for(Sku sku : this.skus) {
            if(!CollectionUtils.isEmpty(sku.getAttrs())) {
               for(SkuAttribute skuAttribute : sku.getAttrs()) {
                  skuAttrKeys.add(skuAttribute.getAttrKey());
               }
            }
         }

         return skuAttrKeys;
      }
   }

   @JsonIgnore
   public List getGeneralSkus() {
      return this.skus;
   }

   public String toString() {
      return "FullItem(item=" + this.getItem() + ", itemDetail=" + this.getItemDetail() + ", skus=" + this.getSkus() + ")";
   }

   public Item getItem() {
      return this.item;
   }

   public void setItem(Item item) {
      this.item = item;
   }

   public ItemDetail getItemDetail() {
      return this.itemDetail;
   }

   public void setItemDetail(ItemDetail itemDetail) {
      this.itemDetail = itemDetail;
   }

   public List getSkus() {
      return this.skus;
   }

   public void setSkus(List skus) {
      this.skus = skus;
   }
}
