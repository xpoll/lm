package cn.blmdz.wolf.rule.attribute;

import com.google.common.base.Supplier;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Maps;
import com.google.common.collect.Multimap;
import com.google.common.collect.Table;
import com.google.common.collect.Tables;

import cn.blmdz.wolf.attribute.dto.GroupedSkuAttribute;
import cn.blmdz.wolf.attribute.dto.SkuAttribute;
import cn.blmdz.wolf.cache.SpuCacher;
import cn.blmdz.wolf.item.dto.FullItem;
import cn.blmdz.wolf.rule.RuleExecutor;
import cn.blmdz.wolf.rule.dto.BaseInput;
import cn.blmdz.wolf.rule.dto.BaseOutput;
import cn.blmdz.wolf.rule.exception.InvalidException;
import cn.blmdz.wolf.spu.dto.FullSpu;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

public class SkuAttributeRuleBySpuExecutor extends RuleExecutor {
   private static final Logger log = LoggerFactory.getLogger(SkuAttributeRuleBySpuExecutor.class);
   private final SpuCacher spuCacher;

   public SkuAttributeRuleBySpuExecutor(SpuCacher spuCacher) {
      this.spuCacher = spuCacher;
   }

   public void doHandleInboundData(BaseInput input, BaseOutput output) throws InvalidException {
      List<GroupedSkuAttribute> rules = this.getRules(input);
      if(!CollectionUtils.isEmpty(rules)) {
         if(CollectionUtils.isEmpty(input.getGroupedSkuAttributes())) {
            log.error("no sku attributes specified");
            throw new InvalidException("sku.attr.missing");
         } else {
            Multimap<String, String> byAttrKey = HashMultimap.create();

            for(GroupedSkuAttribute rule : rules) {
               String attrKey = rule.getAttrKey();

               for(SkuAttribute skuAttribute : rule.getSkuAttributes()) {
                  byAttrKey.put(attrKey, skuAttribute.getAttrVal());
               }
            }

            label162:
            for(GroupedSkuAttribute groupedSkuAttribute : input.getGroupedSkuAttributes()) {
               String attrKey = groupedSkuAttribute.getAttrKey();
               Collection<String> allowedValues = byAttrKey.get(attrKey);
               Iterator i$ = groupedSkuAttribute.getSkuAttributes().iterator();

               String attrVal;
               while(true) {
                  if(!i$.hasNext()) {
                     continue label162;
                  }

                  SkuAttribute skuAttribute = (SkuAttribute)i$.next();
                  attrVal = skuAttribute.getAttrVal();
                  if(!allowedValues.contains(attrVal)) {
                     break;
                  }
               }

               log.error("invalid sku attr value:{} of attr key:{}", attrVal, attrKey);
               throw new InvalidException("sku.attr(key={0},val={1}).invalid", new Object[]{attrKey, attrVal});
            }

         }
      }
   }

   public void doHandleOutboundData(BaseInput input, BaseOutput output) {
      List<GroupedSkuAttribute> groupedSkuAttributesOfSpu = this.getRules(input);
      if(!CollectionUtils.isEmpty(groupedSkuAttributesOfSpu)) {
         List<GroupedSkuAttribute> groupedSkuAttributesOfItem = input.getGroupedSkuAttributes();
         if(!CollectionUtils.isEmpty(groupedSkuAttributesOfItem)) {
            Table<String, String, SkuAttribute> table = Tables.newCustomTable(Maps.<String, Map<String, SkuAttribute>>newHashMap(), new Supplier<Map<String, SkuAttribute>>() {
               public Map<String, SkuAttribute> get() {
                  return Maps.newHashMap();
               }
            });

            for(GroupedSkuAttribute groupedSkuAttribute : groupedSkuAttributesOfItem) {
               String attrKey = groupedSkuAttribute.getAttrKey();

               for(SkuAttribute skuAttribute : groupedSkuAttribute.getSkuAttributes()) {
                  table.put(attrKey, skuAttribute.getAttrVal(), skuAttribute);
               }
            }

            for(GroupedSkuAttribute groupedSkuAttribute : groupedSkuAttributesOfSpu) {
               String attrKey = groupedSkuAttribute.getAttrKey();

               for(SkuAttribute skuAttribute : groupedSkuAttribute.getSkuAttributes()) {
                  SkuAttribute skuAttributeOfItem = (SkuAttribute)table.get(attrKey, skuAttribute.getAttrVal());
                  skuAttribute.setShowImage(skuAttributeOfItem.getShowImage());
                  if(StringUtils.hasText(skuAttributeOfItem.getImage())) {
                     skuAttribute.setImage(skuAttributeOfItem.getImage());
                  }

                  if(StringUtils.hasText(skuAttributeOfItem.getThumbnail())) {
                     skuAttribute.setThumbnail(skuAttributeOfItem.getThumbnail());
                  }
               }
            }
         }

         input.setGroupedSkuAttributes(groupedSkuAttributesOfSpu);
      } else {
         input.setGroupedOtherAttributes(Collections.emptyList());
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

   protected List<GroupedSkuAttribute> getRules(BaseInput data) {
      FullSpu spu = this.spuCacher.findFullSpuById(data.getSpuId());
      return spu.getGroupedSkuAttributes();
   }
}
