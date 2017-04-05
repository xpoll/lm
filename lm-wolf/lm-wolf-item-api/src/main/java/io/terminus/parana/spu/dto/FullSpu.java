package io.terminus.parana.spu.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.google.common.collect.Sets;
import io.terminus.parana.attribute.dto.SkuAttribute;
import io.terminus.parana.rule.dto.BaseInput;
import io.terminus.parana.spu.model.SkuTemplate;
import io.terminus.parana.spu.model.Spu;
import io.terminus.parana.spu.model.SpuDetail;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import org.apache.commons.collections.CollectionUtils;

public class FullSpu extends BaseInput {
   private static final long serialVersionUID = -445616206316005098L;
   private Spu spu;
   private SpuDetail spuDetail;
   private List skuTemplates;

   @JsonIgnore
   public Long getCategoryId() {
      return this.spu != null?this.spu.getCategoryId():null;
   }

   @JsonIgnore
   public Long getSpuId() {
      return this.spu != null?this.spu.getId():null;
   }

   @JsonIgnore
   public Set getSkuAttrKeys() {
      if(CollectionUtils.isEmpty(this.skuTemplates)) {
         return Collections.emptySet();
      } else {
         Set<String> skuAttrKeys = Sets.newLinkedHashSet();

         for(SkuTemplate skuTemplate : this.skuTemplates) {
            if(!CollectionUtils.isEmpty(skuTemplate.getAttrs())) {
               for(SkuAttribute skuAttribute : skuTemplate.getAttrs()) {
                  skuAttrKeys.add(skuAttribute.getAttrKey());
               }
            }
         }

         return skuAttrKeys;
      }
   }

   @JsonIgnore
   public List getGeneralSkus() {
      return this.skuTemplates;
   }

   public String toString() {
      return "FullSpu(spu=" + this.getSpu() + ", spuDetail=" + this.getSpuDetail() + ", skuTemplates=" + this.getSkuTemplates() + ")";
   }

   public Spu getSpu() {
      return this.spu;
   }

   public void setSpu(Spu spu) {
      this.spu = spu;
   }

   public SpuDetail getSpuDetail() {
      return this.spuDetail;
   }

   public void setSpuDetail(SpuDetail spuDetail) {
      this.spuDetail = spuDetail;
   }

   public List getSkuTemplates() {
      return this.skuTemplates;
   }

   public void setSkuTemplates(List skuTemplates) {
      this.skuTemplates = skuTemplates;
   }
}
