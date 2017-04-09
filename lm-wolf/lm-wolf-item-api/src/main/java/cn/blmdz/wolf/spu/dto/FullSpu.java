package cn.blmdz.wolf.spu.dto;

import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.apache.commons.collections.CollectionUtils;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.google.common.collect.Sets;

import cn.blmdz.wolf.attribute.dto.SkuAttribute;
import cn.blmdz.wolf.rule.dto.BaseInput;
import cn.blmdz.wolf.spu.model.SkuTemplate;
import cn.blmdz.wolf.spu.model.Spu;
import cn.blmdz.wolf.spu.model.SpuDetail;

public class FullSpu extends BaseInput {
   private static final long serialVersionUID = -445616206316005098L;
   private Spu spu;
   private SpuDetail spuDetail;
   private List<SkuTemplate> skuTemplates;

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
