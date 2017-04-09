package cn.blmdz.wolf.rule.attribute;

import com.google.common.base.Function;
import com.google.common.base.MoreObjects;
import com.google.common.base.Objects;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import cn.blmdz.wolf.attribute.dto.AttributeMetaKey;
import cn.blmdz.wolf.attribute.dto.GroupedSkuAttribute;
import cn.blmdz.wolf.attribute.dto.SkuAttribute;
import cn.blmdz.wolf.category.model.CategoryAttribute;
import cn.blmdz.wolf.component.dto.attribute.AttributeRule;
import cn.blmdz.wolf.component.dto.attribute.GroupedSkuAttributeWithRule;
import cn.blmdz.wolf.component.dto.attribute.SkuAttributeRule;
import cn.blmdz.wolf.rule.RuleExecutor;
import cn.blmdz.wolf.rule.attribute.AttributeRuleBuilder;
import cn.blmdz.wolf.rule.dto.BaseInput;
import cn.blmdz.wolf.rule.dto.BaseOutput;
import cn.blmdz.wolf.rule.exception.InvalidException;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;

public abstract class SkuAttributeRuleByCategoryExecutor extends RuleExecutor {
   private static final Logger log = LoggerFactory.getLogger(SkuAttributeRuleByCategoryExecutor.class);

   public void doHandleInboundData(BaseInput input, BaseOutput output) throws InvalidException {
      List<?> rules = this.getRules(input);
      List<GroupedSkuAttribute> groupedSkuAttributesOfItemOrSpu = input.getGroupedSkuAttributes();
      if(!CollectionUtils.isEmpty(rules)) {
         if(!CollectionUtils.isEmpty(groupedSkuAttributesOfItemOrSpu)) {
            Map<String, ?> byAttrKey = Maps.uniqueIndex(rules, new Function() {
               public String apply(Object rule) {
                  AttributeRule attributeRule = (AttributeRule)rule;
                  return attributeRule.getAttrKey();
               }
            });

            label62:
            for(GroupedSkuAttribute groupedSkuAttributeOfItemOrSpu : groupedSkuAttributesOfItemOrSpu) {
               String attrKey = groupedSkuAttributeOfItemOrSpu.getAttrKey();
               if(!byAttrKey.keySet().contains(attrKey)) {
                  log.error("attrKey:{} is not allowed", attrKey);
                  throw new InvalidException("sku.attrKey{0}.invalid", new Object[]{attrKey});
               }

               AttributeRule attributeRule = (AttributeRule)byAttrKey.get(attrKey);
               Iterator i$ = groupedSkuAttributeOfItemOrSpu.getSkuAttributes().iterator();

               String attrVal;
               while(true) {
                  if(!i$.hasNext()) {
                     continue label62;
                  }

                  SkuAttribute skuAttribute = (SkuAttribute)i$.next();
                  attrVal = skuAttribute.getAttrVal();
                  List<String> allowedValues = attributeRule.getAttrVals();
                  if(!allowedValues.contains(attrVal) && !userDefinedValueAllowed(attributeRule.getAttrMetas())) {
                     break;
                  }
               }

               log.error("sku attrVal({}) of attrKey({}) not allowed", attrVal, attrKey);
               throw new InvalidException("sku.attr(key={0},val={1}).invalid", new Object[]{attrKey, attrVal});
            }

         }
      }
   }

   public void doHandleOutboundData(BaseInput input, BaseOutput result) {
      List<?> rules = this.getRules(input);
      List<GroupedSkuAttribute> groupedSkuAttributes = input.getGroupedSkuAttributes();
      if(!CollectionUtils.isEmpty(groupedSkuAttributes)) {
         if(!CollectionUtils.isEmpty(rules) || !CollectionUtils.isEmpty(groupedSkuAttributes)) {
            if(CollectionUtils.isEmpty(rules) && !CollectionUtils.isEmpty(groupedSkuAttributes)) {
               result.setSkuAttrs(Lists.newArrayList());
            } else {
               List<GroupedSkuAttributeWithRule> groupedSkuAttributeWithRules = Lists.newArrayList();

               for(GroupedSkuAttribute groupedSkuAttribute : groupedSkuAttributes) {
                  String attrKey = groupedSkuAttribute.getAttrKey();

                  for(Object rule : rules) {
                     AttributeRule attributeRule = (AttributeRule)rule;
                     Map<AttributeMetaKey, String> attrMetas = attributeRule.getAttrMetas();
                     if(Objects.equal(attributeRule.getAttrKey(), attrKey)) {
                        List<String> definedValues = (List)MoreObjects.firstNonNull(attributeRule.getAttrVals(), Collections.emptyList());
                        List<SkuAttributeRule> skuAttributeRules = Lists.newArrayList();

                        for(SkuAttribute skuAttribute : groupedSkuAttribute.getSkuAttributes()) {
                           String attrVal = skuAttribute.getAttrVal();
                           if(definedValues.contains(attrVal) || userDefinedValueAllowed(attrMetas)) {
                              SkuAttributeRule skuAttributeRule = new SkuAttributeRule();
                              skuAttributeRule.setAttrMetas(attrMetas);
                              skuAttributeRule.setAttrVal(attrVal);
                              skuAttributeRule.setUnit(skuAttribute.getUnit());
                              skuAttributeRule.setImage(skuAttribute.getImage());
                              skuAttributeRule.setShowImage(skuAttribute.getShowImage());
                              skuAttributeRule.setThumbnail(skuAttribute.getThumbnail());
                              skuAttributeRules.add(skuAttributeRule);
                           }
                        }

                        if(!CollectionUtils.isEmpty(skuAttributeRules)) {
                           GroupedSkuAttributeWithRule gsaw = new GroupedSkuAttributeWithRule();
                           gsaw.setAttrKey(attrKey);
                           gsaw.setAttributeRules(skuAttributeRules);
                           groupedSkuAttributeWithRules.add(gsaw);
                        }
                     }
                  }
               }

               result.setSkuAttrs(groupedSkuAttributeWithRules);
            }
         }
      }
   }

   protected abstract List getCategoryAttributes(Long var1);

   protected List getRules(BaseInput data) {
      List<CategoryAttribute> categoryAttributes = this.getCategoryAttributes(data.getCategoryId());
      return CollectionUtils.isEmpty(categoryAttributes)?Collections.emptyList():AttributeRuleBuilder.buildSkuAttributeRule(categoryAttributes);
   }
}
