package cn.blmdz.wolf.parana.rule.sku;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;

import cn.blmdz.wolf.parana.attribute.dto.SkuAttribute;
import cn.blmdz.wolf.parana.cache.SpuCacher;
import cn.blmdz.wolf.parana.component.dto.item.EditItem;
import cn.blmdz.wolf.parana.item.dto.FullItem;
import cn.blmdz.wolf.parana.item.model.Sku;
import cn.blmdz.wolf.parana.rule.RuleExecutor;
import cn.blmdz.wolf.parana.rule.dto.BaseInput;
import cn.blmdz.wolf.parana.rule.dto.BaseOutput;
import cn.blmdz.wolf.parana.rule.dto.GeneralSku;
import cn.blmdz.wolf.parana.rule.exception.InvalidException;
import cn.blmdz.wolf.parana.spu.dto.FullSpu;
import cn.blmdz.wolf.parana.spu.model.SkuTemplate;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;

public class SkuRuleBySpuExecutor extends RuleExecutor {
   private static final Logger log = LoggerFactory.getLogger(SkuRuleBySpuExecutor.class);
   private final SpuCacher spuCacher;

   public SkuRuleBySpuExecutor(SpuCacher spuCacher) {
      this.spuCacher = spuCacher;
   }

   public void doHandleInboundData(BaseInput input, BaseOutput output) throws InvalidException {
      List<SkuTemplate> skuTemplates = this.getRules(input);
      if(!CollectionUtils.isEmpty(skuTemplates) || !CollectionUtils.isEmpty(input.getGeneralSkus())) {
         if(CollectionUtils.isEmpty(skuTemplates) && !CollectionUtils.isEmpty(input.getGeneralSkus())) {
            log.error("no sku allowed for spu(id={})", input.getSpuId());
            throw new InvalidException("sku.not.permit");
         } else if(!CollectionUtils.isEmpty(skuTemplates) && CollectionUtils.isEmpty(input.getGeneralSkus())) {
            log.error("sku required for spu(id={})", input.getSpuId());
            throw new InvalidException("sku.missing");
         } else {
            Multimap<String, String> allowedSkuAttributes = HashMultimap.create();

            for(SkuTemplate skuTemplate : skuTemplates) {
               if(!CollectionUtils.isEmpty(skuTemplate.getAttrs())) {
                  for(SkuAttribute skuAttribute : skuTemplate.getAttrs()) {
                     allowedSkuAttributes.put(skuAttribute.getAttrKey(), skuAttribute.getAttrVal());
                  }
               }
            }

            label231:
            for(GeneralSku generalSku : input.getGeneralSkus()) {
               Sku sku = (Sku)generalSku;
               if(!CollectionUtils.isEmpty(sku.getAttrs())) {
                  Iterator i$ = sku.getAttrs().iterator();

                  String attrKey;
                  String attrVal;
                  while(true) {
                     if(!i$.hasNext()) {
                        continue label231;
                     }

                     SkuAttribute skuAttribute = (SkuAttribute)i$.next();
                     attrKey = skuAttribute.getAttrKey();
                     if(!allowedSkuAttributes.containsKey(attrKey)) {
                        log.error("sku attr key({}) not allowed", attrKey);
                        throw new InvalidException("sku.attr(key={0}).invalid", new Object[]{attrKey});
                     }

                     attrVal = skuAttribute.getAttrVal();
                     Collection<String> allowedValues = allowedSkuAttributes.get(attrKey);
                     if(!allowedValues.contains(attrVal)) {
                        break;
                     }
                  }

                  log.error("sku attr val({}) of sku attr key ({}) is invalid", attrVal, attrKey);
                  throw new InvalidException("sku.attr(key={0},val={1)).invalid", new Object[]{attrKey, attrVal});
               }
            }

         }
      }
   }

   public void doHandleOutboundData(BaseInput input, BaseOutput output) {
      List<SkuTemplate> skuTemplates = this.getRules(input);
      if(!CollectionUtils.isEmpty(skuTemplates) || !CollectionUtils.isEmpty(input.getGeneralSkus())) {
         if(CollectionUtils.isEmpty(skuTemplates) && !CollectionUtils.isEmpty(input.getGeneralSkus())) {
            log.error("no sku allowed for spu(id={})", input.getSpuId());
            ((EditItem)output).setSkus(Collections.emptyList());
         }

         if(!CollectionUtils.isEmpty(skuTemplates) && CollectionUtils.isEmpty(input.getGeneralSkus())) {
            log.error("sku of item missing because spu(id={}) required", input.getSpuId());
         } else {
            Multimap<String, String> allowedSkuAttributes = HashMultimap.create();

            for(SkuTemplate skuTemplate : skuTemplates) {
               if(!CollectionUtils.isEmpty(skuTemplate.getAttrs())) {
                  for(SkuAttribute skuAttribute : skuTemplate.getAttrs()) {
                     allowedSkuAttributes.put(skuAttribute.getAttrKey(), skuAttribute.getAttrVal());
                  }
               }
            }

            List<Sku> validSkus = Lists.newArrayList();

            for(GeneralSku generalSku : input.getGeneralSkus()) {
               Sku sku = (Sku)generalSku;
               boolean valid = true;
               if(!CollectionUtils.isEmpty(sku.getAttrs())) {
                  for(SkuAttribute skuAttribute : sku.getAttrs()) {
                     String attrKey = skuAttribute.getAttrKey();
                     if(!allowedSkuAttributes.containsKey(attrKey)) {
                        log.error("sku attr key({}) not allowed", attrKey);
                        valid = false;
                        break;
                     }

                     String attrVal = skuAttribute.getAttrVal();
                     Collection<String> allowedValues = allowedSkuAttributes.get(attrKey);
                     if(!allowedValues.contains(attrVal)) {
                        log.error("sku attr val({}) of sku attr key ({}) is invalid", attrVal, attrKey);
                        valid = false;
                        break;
                     }
                  }
               }

               if(valid) {
                  validSkus.add(sku);
               }
            }

            ((EditItem)output).setSkus(validSkus);
         }
      }
   }

   public boolean support(BaseInput input) {
      if(!(input instanceof FullItem)) {
         return false;
      } else if(input.getSpuId() == null) {
         return false;
      } else {
         FullSpu fullSpu = this.spuCacher.findFullSpuById(input.getSpuId());
         return fullSpu.getSpu().getStatus().intValue() >= 0;
      }
   }

   protected List getRules(BaseInput data) {
      Long spuId = data.getSpuId();
      FullSpu fullSpu = this.spuCacher.findFullSpuById(spuId);
      return fullSpu.getSkuTemplates();
   }
}
