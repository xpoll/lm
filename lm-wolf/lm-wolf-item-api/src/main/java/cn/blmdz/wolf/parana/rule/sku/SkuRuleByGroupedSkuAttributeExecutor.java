package cn.blmdz.wolf.parana.rule.sku;

import com.google.common.base.Objects;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;

import cn.blmdz.wolf.parana.attribute.dto.GroupedSkuAttribute;
import cn.blmdz.wolf.parana.attribute.dto.SkuAttribute;
import cn.blmdz.wolf.parana.item.dto.FullItem;
import cn.blmdz.wolf.parana.rule.RuleExecutor;
import cn.blmdz.wolf.parana.rule.dto.BaseInput;
import cn.blmdz.wolf.parana.rule.dto.BaseOutput;
import cn.blmdz.wolf.parana.rule.dto.GeneralSku;
import cn.blmdz.wolf.parana.rule.exception.InvalidException;
import cn.blmdz.wolf.parana.spu.dto.FullSpu;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;

public class SkuRuleByGroupedSkuAttributeExecutor extends RuleExecutor {
   private static final Logger log = LoggerFactory.getLogger(SkuRuleByGroupedSkuAttributeExecutor.class);

   public void doHandleInboundData(BaseInput input, BaseOutput output) throws InvalidException {
      List<GroupedSkuAttribute> rules = this.getRules(input);
      Multimap<String, SkuAttribute> byAttrKey = HashMultimap.create();
      if(!CollectionUtils.isEmpty(rules)) {
         for(GroupedSkuAttribute rule : rules) {
            String attrKey = rule.getAttrKey();

            for(SkuAttribute skuAttribute : rule.getSkuAttributes()) {
               byAttrKey.put(attrKey, skuAttribute);
            }
         }
      }

      if(CollectionUtils.isEmpty(input.getGeneralSkus())) {
         log.error("no sku specified for {}", input);
         throw new InvalidException("sku.missing");
      } else {
         label159:
         for(GeneralSku generalSku : input.getGeneralSkus()) {
            List<SkuAttribute> skuAttributes = generalSku.getAttrs();
            if(!CollectionUtils.isEmpty(skuAttributes) || !byAttrKey.isEmpty()) {
               if(CollectionUtils.isEmpty(skuAttributes) && !byAttrKey.isEmpty()) {
                  log.error("no sku attribute for {}", generalSku);
                  throw new InvalidException("sku.attribute.missing");
               }

               Iterator i$ = skuAttributes.iterator();

               SkuAttribute skuAttribute;
               while(true) {
                  if(!i$.hasNext()) {
                     continue label159;
                  }

                  skuAttribute = (SkuAttribute)i$.next();
                  String attrKey = skuAttribute.getAttrKey();
                  if(!byAttrKey.containsKey(attrKey)) {
                     log.error("sku attrKey:{} is invalid", attrKey);
                     throw new InvalidException("invalid.sku.attrKey:{0}", new Object[]{attrKey});
                  }

                  Collection<SkuAttribute> allowedValues = byAttrKey.get(attrKey);
                  boolean valid = false;

                  for(SkuAttribute allowedValue : allowedValues) {
                     if(Objects.equal(allowedValue.getAttrVal(), skuAttribute.getAttrVal()) && Objects.equal(allowedValue.getUnit(), skuAttribute.getUnit())) {
                        valid = true;
                        break;
                     }
                  }

                  if(!valid) {
                     break;
                  }
               }

               log.error("invalid {}", skuAttribute);
               throw new InvalidException("invalid.sku.attr:{0}", new Object[]{skuAttribute});
            }
         }

      }
   }

   public void doHandleOutboundData(BaseInput input, BaseOutput output) {
      if(!CollectionUtils.isEmpty(input.getGeneralSkus())) {
         List<GroupedSkuAttribute> rules = this.getRules(input);
         if(CollectionUtils.isEmpty(rules)) {
            GeneralSku generalSku = (GeneralSku)input.getGeneralSkus().get(0);
            if(!CollectionUtils.isEmpty(generalSku.getAttrs())) {
               generalSku.getAttrs().clear();
            }

            output.setGeneralSku(Lists.newArrayList(new GeneralSku[]{generalSku}));
         } else {
            Multimap<String, SkuAttribute> byAttrKey = HashMultimap.create();

            for(GroupedSkuAttribute rule : rules) {
               String attrKey = rule.getAttrKey();

               for(SkuAttribute skuAttribute : rule.getSkuAttributes()) {
                  byAttrKey.put(attrKey, skuAttribute);
               }
            }

            List<GeneralSku> result = Lists.newArrayListWithCapacity(input.getGeneralSkus().size());

            for(GeneralSku generalSku : input.getGeneralSkus()) {
               List<SkuAttribute> skuAttributes = generalSku.getAttrs();
               if(!CollectionUtils.isEmpty(skuAttributes)) {
                  boolean valid = true;

                  label264:
                  for(SkuAttribute skuAttribute : skuAttributes) {
                     String attrKey = skuAttribute.getAttrKey();
                     if(!byAttrKey.containsKey(attrKey)) {
                        valid = false;
                        break;
                     }

                     Collection<SkuAttribute> allowedValues = byAttrKey.get(attrKey);
                     valid = false;
                     Iterator i$ = allowedValues.iterator();

                     while(true) {
                        if(!i$.hasNext()) {
                           continue label264;
                        }

                        SkuAttribute allowedValue = (SkuAttribute)i$.next();
                        if(Objects.equal(allowedValue.getAttrVal(), skuAttribute.getAttrVal()) && Objects.equal(allowedValue.getUnit(), skuAttribute.getUnit())) {
                           break;
                        }
                     }

                     valid = true;
                  }

                  if(valid) {
                     result.add(generalSku);
                  }
               }
            }

            output.setGeneralSku(result);
         }
      }
   }

   public boolean support(BaseInput input) {
      return input instanceof FullItem || input instanceof FullSpu;
   }

   protected List getRules(BaseInput data) {
      return data.getGroupedSkuAttributes();
   }
}
