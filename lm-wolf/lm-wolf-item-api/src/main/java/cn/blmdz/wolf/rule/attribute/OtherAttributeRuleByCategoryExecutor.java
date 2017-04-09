package cn.blmdz.wolf.rule.attribute;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import com.google.common.base.Function;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multimaps;

import cn.blmdz.wolf.attribute.dto.AttributeMetaKey;
import cn.blmdz.wolf.attribute.dto.GroupedOtherAttribute;
import cn.blmdz.wolf.attribute.dto.OtherAttribute;
import cn.blmdz.wolf.attribute.dto.PreservedGroup;
import cn.blmdz.wolf.category.model.CategoryAttribute;
import cn.blmdz.wolf.component.dto.attribute.AttributeRule;
import cn.blmdz.wolf.component.dto.attribute.GroupedOtherAttributeWithRule;
import cn.blmdz.wolf.component.dto.attribute.OtherAttributeWithRule;
import cn.blmdz.wolf.rule.RuleExecutor;
import cn.blmdz.wolf.rule.dto.BaseInput;
import cn.blmdz.wolf.rule.dto.BaseOutput;
import cn.blmdz.wolf.rule.exception.InvalidException;

public abstract class OtherAttributeRuleByCategoryExecutor extends RuleExecutor {
   private static final Logger log = LoggerFactory.getLogger(OtherAttributeRuleByCategoryExecutor.class);

   public void doHandleInboundData(BaseInput input, BaseOutput baseOutput) throws InvalidException {
      List<?> rules = this.getRules(input);
      List<GroupedOtherAttribute> groupedOtherAttributesOfItemOrSpu = input.getGroupedOtherAttributes();
      if(!CollectionUtils.isEmpty(groupedOtherAttributesOfItemOrSpu) || !CollectionUtils.isEmpty(rules)) {
         if(CollectionUtils.isEmpty(groupedOtherAttributesOfItemOrSpu) && !CollectionUtils.isEmpty(rules)) {
            for(Object rule : rules) {
               AttributeRule attributeRule = (AttributeRule)rule;
               Map<AttributeMetaKey, String> metas = attributeRule.getAttrMetas();
               String requiredMetaVal = (String)metas.get(AttributeMetaKey.REQUIRED);
               if(StringUtils.hasText(requiredMetaVal) && Boolean.valueOf(requiredMetaVal).booleanValue()) {
                  log.error("missing required other attr({})", attributeRule.getAttrKey());
                  throw new InvalidException(400, "other.attrKey({0}).missing", new Object[]{attributeRule.getAttrKey()});
               }
            }

         } else {
            Map<String, OtherAttribute> byAttrKey = Maps.newHashMap();

            for(GroupedOtherAttribute groupedOtherAttribute : groupedOtherAttributesOfItemOrSpu) {
               for(OtherAttribute otherAttribute : groupedOtherAttribute.getOtherAttributes()) {
                  byAttrKey.put(otherAttribute.getAttrKey(), otherAttribute);
               }
            }

            if(!CollectionUtils.isEmpty(rules)) {
               label329:
               for(Object rule : rules) {
                  AttributeRule otherAttributeRule = (AttributeRule)rule;
                  String attrKey = otherAttributeRule.getAttrKey();
                  OtherAttribute otherAttr = (OtherAttribute)byAttrKey.remove(attrKey);
                  String attrVal = otherAttr != null?otherAttr.getAttrVal():null;
                  Map<AttributeMetaKey, String> metas = otherAttributeRule.getAttrMetas();
                  List<String> allowedValues = otherAttributeRule.getAttrVals();
                  Iterator i$ = metas.keySet().iterator();

                  AttributeMetaKey attributeMetaKey;
                  while(true) {
                     if(!i$.hasNext()) {
                        continue label329;
                     }

                     attributeMetaKey = (AttributeMetaKey)i$.next();
                     if(!attributeMetaKey.validate(attrVal, (String)metas.get(attributeMetaKey), allowedValues)) {
                        break;
                     }
                  }

                  String metaValue = (String)metas.get(attributeMetaKey);
                  log.error("{} is invalid, cause: rules meta ({}) expect {}, but actual attr value is {}", new Object[]{otherAttributeRule, attributeMetaKey.name(), metaValue, attrVal});
                  throw new InvalidException(400, "invalid.other.attr(key={0}, meta={1}, val={2})", new Object[]{otherAttributeRule.getAttrKey(), attributeMetaKey, attrVal});
               }
            }

            List<OtherAttribute> remainderAttributes = Lists.newArrayList(byAttrKey.values());
            if(!CollectionUtils.isEmpty(remainderAttributes)) {
               for(OtherAttribute remainderAttribute : remainderAttributes) {
                  String group = remainderAttribute.getGroup();
                  if(!PreservedGroup.contains(group)) {
                     remainderAttribute.setGroup(PreservedGroup.USER_DEFINED.name());
                  }
               }
            }

         }
      }
   }

   public void doHandleOutboundData(BaseInput input, BaseOutput result) {
      List<GroupedOtherAttribute> groupedOtherAttributesOfItemOrSpu = input.getGroupedOtherAttributes();
      List<?> rules = this.getRules(input);
      if(!CollectionUtils.isEmpty(groupedOtherAttributesOfItemOrSpu) || !CollectionUtils.isEmpty(rules)) {
         Map<String, OtherAttribute> attrKeyMap = Maps.newHashMap();
         if(!CollectionUtils.isEmpty(groupedOtherAttributesOfItemOrSpu)) {
            for(GroupedOtherAttribute groupedOtherAttribute : groupedOtherAttributesOfItemOrSpu) {
               for(OtherAttribute otherAttribute : groupedOtherAttribute.getOtherAttributes()) {
                  attrKeyMap.put(otherAttribute.getAttrKey(), otherAttribute);
               }
            }
         }

         List<OtherAttributeWithRule> otherAttributeWithRules = Lists.newArrayList();

         for(Object rule : rules) {
            AttributeRule otherAttributeRule = (AttributeRule)rule;
            List<String> attrVals = otherAttributeRule.getAttrVals();
            if(CollectionUtils.isEmpty(attrVals)) {
               attrVals = Lists.newArrayList();
               otherAttributeRule.setAttrVals(attrVals);
            }

            OtherAttributeWithRule otherAttributeWithRule = new OtherAttributeWithRule();
            otherAttributeWithRule.setAttributeRule(otherAttributeRule);
            OtherAttribute otherAttribute = (OtherAttribute)attrKeyMap.remove(otherAttributeRule.getAttrKey());
            if(otherAttribute != null) {
               String attrVal = otherAttribute.getAttrVal();
               otherAttributeWithRule.setAttrVal(attrVal);
               otherAttributeWithRule.setReadOnlyBySeller(otherAttribute.getReadOnlyBySeller());
               if(userDefinedValueAllowed(otherAttributeRule.getAttrMetas()) && !attrVals.contains(attrVal)) {
                  attrVals.add(attrVal);
               }
            }

            otherAttributeWithRules.add(otherAttributeWithRule);
         }

         for(OtherAttribute otherAttribute : attrKeyMap.values()) {
            OtherAttributeWithRule otherAttributeWithRule = new OtherAttributeWithRule();
            otherAttributeWithRule.setAttrVal(otherAttribute.getAttrVal());
            otherAttributeWithRule.setReadOnlyBySeller(otherAttribute.getReadOnlyBySeller());
            AttributeRule attributeRule = new AttributeRule();
            if(!PreservedGroup.contains(otherAttribute.getGroup())) {
               attributeRule.setGroup(PreservedGroup.USER_DEFINED.name());
            } else {
               attributeRule.setGroup(otherAttribute.getGroup());
            }

            attributeRule.setAttrKey(otherAttribute.getAttrKey());
            otherAttributeWithRule.setAttributeRule(attributeRule);
            otherAttributeWithRules.add(otherAttributeWithRule);
         }

         List<GroupedOtherAttributeWithRule> groupedOtherAttributeWithRules = this.groupOtherAttributeWithRules(otherAttributeWithRules);
         result.setOtherAttrs(groupedOtherAttributeWithRules);
      }
   }

   protected abstract List getCategoryAttributes(Long var1);

   protected List getRules(BaseInput data) {
      List<CategoryAttribute> categoryAttributes = this.getCategoryAttributes(data.getCategoryId());
      if(CollectionUtils.isEmpty(categoryAttributes)) {
         return Collections.emptyList();
      } else {
         List<AttributeRule> attributeRules = AttributeRuleBuilder.buildAttributeRuleFromCategoryAttributes(categoryAttributes);
         Set<String> skuAttrKeys = data.getSkuAttrKeys();
         List<Object> result = Lists.newArrayListWithCapacity(attributeRules.size());

         for(AttributeRule attributeRule : attributeRules) {
            String attrKey = attributeRule.getAttrKey();
            if(!skuAttrKeys.contains(attrKey)) {
               result.add(attributeRule);
            }
         }

         return result;
      }
   }

   private List groupOtherAttributeWithRules(List otherAttributeWithRules) {
      Multimap<String, OtherAttributeWithRule> byGroup = Multimaps.index(otherAttributeWithRules, new Function<OtherAttributeWithRule, String>() {
         public String apply(OtherAttributeWithRule otherAttributeWithRule) {
            return otherAttributeWithRule.getAttributeRule().getGroup();
         }
      });
      List<GroupedOtherAttributeWithRule> result = Lists.newArrayList();

      for(String group : byGroup.keySet()) {
         GroupedOtherAttributeWithRule groupedRule = new GroupedOtherAttributeWithRule();
         groupedRule.setGroup(group);
         List<OtherAttributeWithRule> rules = Lists.newArrayList();
         rules.addAll(byGroup.get(group));
         groupedRule.setOtherAttributeWithRules(rules);
         result.add(groupedRule);
      }

      return result;
   }
}
