package cn.blmdz.wolf.rule.sku;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;

import com.google.common.base.Function;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import cn.blmdz.wolf.attribute.dto.SkuAttribute;
import cn.blmdz.wolf.category.model.CategoryAttribute;
import cn.blmdz.wolf.component.dto.attribute.AttributeRule;
import cn.blmdz.wolf.rule.RuleExecutor;
import cn.blmdz.wolf.rule.attribute.AttributeRuleBuilder;
import cn.blmdz.wolf.rule.dto.BaseInput;
import cn.blmdz.wolf.rule.dto.BaseOutput;
import cn.blmdz.wolf.rule.dto.GeneralSku;
import cn.blmdz.wolf.rule.exception.InvalidException;

public abstract class GeneralSkuRuleByCategoryExecutor extends RuleExecutor {
   private static final Logger log = LoggerFactory.getLogger(GeneralSkuRuleByCategoryExecutor.class);

   public void doHandleInboundData(BaseInput input, BaseOutput baseOutput) throws InvalidException {
      List<?> rules = this.getRules(input);
      Map<String, ?> byAttrKey = Maps.uniqueIndex(rules, new Function() {
         public String apply(Object rule) {
            return ((AttributeRule)rule).getAttrKey();
         }
      });
      if(CollectionUtils.isEmpty(rules) && input.getGeneralSkus().size() > 1) {
         log.error("multiple sku not allowed, since no sku attribute rule defined by category(id={}) ", input.getCategoryId());
         throw new InvalidException("single.sku.allowed");
      } else {
         label85:
         for(GeneralSku generalSku : input.getGeneralSkus()) {
            List<SkuAttribute> skuAttributes = generalSku.getAttrs();
            if(!CollectionUtils.isEmpty(rules) && CollectionUtils.isEmpty(skuAttributes)) {
               return;
            }

            if(CollectionUtils.isEmpty(rules) && CollectionUtils.isEmpty(skuAttributes)) {
               return;
            }

            Iterator i$ = skuAttributes.iterator();

            SkuAttribute skuAttribute;
            while(true) {
               if(!i$.hasNext()) {
                  continue label85;
               }

               skuAttribute = (SkuAttribute)i$.next();
               String attrKey = skuAttribute.getAttrKey();
               if(!byAttrKey.containsKey(attrKey)) {
                  log.error("sku attribute key ({}) is invalid", attrKey);
                  throw new InvalidException("sku.attr(key={0}).invalid", new Object[]{attrKey});
               }

               AttributeRule attributeRule = (AttributeRule)byAttrKey.get(attrKey);
               if(!attributeRule.getAttrVals().contains(skuAttribute.getAttrVal()) && !userDefinedValueAllowed(attributeRule.getAttrMetas())) {
                  break;
               }
            }

            log.error("sku attribute (key={},val={})\'s value is invalid", skuAttribute.getAttrKey(), skuAttribute.getAttrVal());
            throw new InvalidException("sku.attr(key={0},val={1}).invalid", new Object[]{skuAttribute.getAttrKey(), skuAttribute.getAttrVal()});
         }

      }
   }

   public void doHandleOutboundData(BaseInput input, BaseOutput result) {
      List<?> rules = this.getRules(input);
      Map<String, ?> byAttrKey = Maps.uniqueIndex(rules, new Function() {
         public String apply(Object rule) {
            return ((AttributeRule)rule).getAttrKey();
         }
      });
      List<GeneralSku> generalSkus = Lists.newArrayListWithCapacity(input.getGeneralSkus().size());

      for(GeneralSku generalSku : input.getGeneralSkus()) {
         List<SkuAttribute> skuAttributes = generalSku.getAttrs();
         if(CollectionUtils.isEmpty(skuAttributes)) {
            generalSkus.add(generalSku);
         } else {
            boolean valid = true;

            for(SkuAttribute skuAttribute : skuAttributes) {
               String attrKey = skuAttribute.getAttrKey();
               if(!byAttrKey.containsKey(attrKey)) {
                  log.error("sku attribute key ({}) is invalid", attrKey);
                  valid = false;
                  break;
               }

               AttributeRule attributeRule = (AttributeRule)byAttrKey.get(attrKey);
               if(!attributeRule.getAttrVals().contains(skuAttribute.getAttrVal()) && !userDefinedValueAllowed(attributeRule.getAttrMetas())) {
                  log.error("sku attribute (key={},val={})\'s value is invalid", skuAttribute.getAttrKey(), skuAttribute.getAttrVal());
                  valid = false;
                  break;
               }
            }

            if(valid) {
               generalSkus.add(generalSku);
            }
         }
      }

      result.setGeneralSku(generalSkus);
   }

   protected abstract List getCategoryAttributes(Long var1);

   protected List getRules(BaseInput data) {
      List<CategoryAttribute> categoryAttributes = this.getCategoryAttributes(data.getCategoryId());
      return CollectionUtils.isEmpty(categoryAttributes)?Collections.emptyList():AttributeRuleBuilder.buildSkuAttributeRule(categoryAttributes);
   }
}
