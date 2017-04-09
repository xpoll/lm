package cn.blmdz.wolf.rule.attribute;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.springframework.util.CollectionUtils;

import com.google.common.base.MoreObjects;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import cn.blmdz.wolf.attribute.dto.AttributeMetaKey;
import cn.blmdz.wolf.category.model.CategoryAttribute;
import cn.blmdz.wolf.component.dto.attribute.AttributeRule;

public class AttributeRuleBuilder {
   public static List buildSkuAttributeRule(List<CategoryAttribute> categoryAttributes) {
      if(CollectionUtils.isEmpty(categoryAttributes)) {
         return Collections.emptyList();
      } else {
         List<AttributeRule> result = Lists.newArrayList();

         for(CategoryAttribute categoryAttribute : categoryAttributes) {
            if(!CollectionUtils.isEmpty(categoryAttribute.getAttrMetas())) {
               String metaValue = (String)categoryAttribute.getAttrMetas().get(AttributeMetaKey.SKU_CANDIDATE);
               if(Boolean.valueOf(metaValue).booleanValue()) {
                  AttributeRule attributeRule = makeAttributeRule(categoryAttribute);
                  result.add(attributeRule);
               }
            }
         }

         return result;
      }
   }

   private static AttributeRule makeAttributeRule(CategoryAttribute categoryAttribute) {
      AttributeRule attributeRule = new AttributeRule();
      attributeRule.setAttrKey(categoryAttribute.getAttrKey());
      attributeRule.setAttrMetas((Map)MoreObjects.firstNonNull(categoryAttribute.getAttrMetas(), Maps.newLinkedHashMap()));
      attributeRule.setAttrVals((List)MoreObjects.firstNonNull(categoryAttribute.getAttrVals(), Lists.newArrayList()));
      attributeRule.setGroup(categoryAttribute.getGroup());
      return attributeRule;
   }

   public static List<AttributeRule> buildAttributeRuleFromCategoryAttributes(List<CategoryAttribute> categoryAttributes) {
      if(CollectionUtils.isEmpty(categoryAttributes)) {
         return Collections.emptyList();
      } else {
         List<AttributeRule> result = Lists.newArrayListWithCapacity(categoryAttributes.size());

         for(CategoryAttribute categoryAttribute : categoryAttributes) {
            AttributeRule attributeRule = makeAttributeRule(categoryAttribute);
            result.add(attributeRule);
         }

         return result;
      }
   }
}
