package io.terminus.parana.rule.attribute;

import com.google.common.base.Objects;
import com.google.common.base.Supplier;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Table;
import com.google.common.collect.Tables;
import io.terminus.parana.attribute.dto.GroupedOtherAttribute;
import io.terminus.parana.attribute.dto.OtherAttribute;
import io.terminus.parana.attribute.dto.PreservedGroup;
import io.terminus.parana.cache.SpuCacher;
import io.terminus.parana.component.dto.attribute.AttributeRule;
import io.terminus.parana.component.dto.attribute.OtherAttributeWithRule;
import io.terminus.parana.item.dto.FullItem;
import io.terminus.parana.rule.RuleExecutor;
import io.terminus.parana.rule.dto.BaseInput;
import io.terminus.parana.rule.dto.BaseOutput;
import io.terminus.parana.rule.exception.InvalidException;
import io.terminus.parana.spu.dto.FullSpu;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;

public class OtherAttributeRuleBySpuExecutor extends RuleExecutor {
   private static final Logger log = LoggerFactory.getLogger(OtherAttributeRuleBySpuExecutor.class);
   private final SpuCacher spuCacher;

   public OtherAttributeRuleBySpuExecutor(SpuCacher spuCacher) {
      this.spuCacher = spuCacher;
   }

   public void doHandleInboundData(BaseInput input, BaseOutput output) throws InvalidException {
      List<?> rules = this.getRules(input);
      List<GroupedOtherAttribute> groupedOtherAttributesOfItem = input.getGroupedOtherAttributes();
      Table<String, String, String> table = Tables.newCustomTable(Maps.newHashMap(), new Supplier() {
         public Map get() {
            return Maps.newHashMap();
         }
      });

      for(GroupedOtherAttribute groupedOtherAttribute : groupedOtherAttributesOfItem) {
         String group = groupedOtherAttribute.getGroup();

         for(OtherAttribute otherAttribute : groupedOtherAttribute.getOtherAttributes()) {
            String attrKey = otherAttribute.getAttrKey();
            String attrVal = otherAttribute.getAttrVal();
            table.put(group, attrKey, attrVal);
         }
      }

      for(Object rule : rules) {
         AttributeRule attributeRule = (AttributeRule)rule;
         String group = attributeRule.getGroup();
         String attrKey = attributeRule.getAttrKey();
         String attrVal = (String)attributeRule.getAttrVals().get(0);
         if(!table.containsRow(group)) {
            log.error("other attribute group({}) missing", group);
            throw new InvalidException("attr.group({0}).missing", new Object[]{group});
         }

         if(!table.contains(group, attrKey)) {
            log.error("other attrKey({}) missing", attrKey);
            throw new InvalidException("other.attrKey{0}.missing", new Object[]{attrKey});
         }

         String actualVal = (String)table.get(group, attrKey);
         if(!Objects.equal(actualVal, attrVal)) {
            log.error("invalid other attribute(key={}, val={}), expect val={}", new Object[]{attrKey, actualVal, attrVal});
            throw new InvalidException("other.attr(key={0},val={1}).invalid", new Object[]{attrKey, actualVal});
         }
      }

   }

   public void doHandleOutboundData(BaseInput input, BaseOutput result) {
      List<?> rules = this.getRules(input);
      List<GroupedOtherAttribute> groupedOtherAttributesOfItem = input.getGroupedOtherAttributes();
      Table<String, String, String> table = Tables.newCustomTable(Maps.newLinkedHashMap(), new Supplier() {
         public Map get() {
            return Maps.newLinkedHashMap();
         }
      });

      for(Object rule : rules) {
         AttributeRule attributeRule = (AttributeRule)rule;
         table.put(attributeRule.getGroup(), attributeRule.getAttrKey(), attributeRule.getAttrVals().get(0));
      }

      for(GroupedOtherAttribute groupedOtherAttribute : groupedOtherAttributesOfItem) {
         String group = groupedOtherAttribute.getGroup();

         for(OtherAttribute otherAttribute : groupedOtherAttribute.getOtherAttributes()) {
            String attrKey = otherAttribute.getAttrKey();
            String attrVal = otherAttribute.getAttrVal();
            if(table.containsRow(group)) {
               if(!table.contains(group, attrKey)) {
                  table.put(group, attrKey, attrVal);
               }
            } else if(!PreservedGroup.contains(group)) {
               table.put(PreservedGroup.USER_DEFINED.name(), attrKey, attrVal);
            }
         }
      }

      List<GroupedOtherAttribute> groupedOtherAttributes = Lists.newArrayList();

      for(String group : table.rowKeySet()) {
         GroupedOtherAttribute groupedOtherAttribute = new GroupedOtherAttribute();
         groupedOtherAttribute.setGroup(group);
         List<OtherAttribute> otherAttributes = Lists.newArrayList();
         Map<String, String> row = table.row(group);

         for(String attrKey : row.keySet()) {
            OtherAttribute otherAttribute = new OtherAttribute();
            otherAttribute.setAttrKey(attrKey);
            otherAttribute.setAttrVal((String)row.get(attrKey));
            otherAttribute.setReadOnlyBySeller(Boolean.TRUE);
            otherAttribute.setGroup(group);
            otherAttributes.add(otherAttribute);
         }

         groupedOtherAttribute.setOtherAttributes(otherAttributes);
         groupedOtherAttributes.add(groupedOtherAttribute);
      }

      input.setGroupedOtherAttributes(groupedOtherAttributes);
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
      List<GroupedOtherAttribute> groupedOtherAttributes = fullSpu.getGroupedOtherAttributes();
      if(CollectionUtils.isEmpty(groupedOtherAttributes)) {
         return Collections.emptyList();
      } else {
         List<Object> otherAttributeRules = Lists.newArrayList();

         for(GroupedOtherAttribute groupedOtherAttribute : groupedOtherAttributes) {
            for(OtherAttribute otherAttribute : groupedOtherAttribute.getOtherAttributes()) {
               AttributeRule attributeRule = new AttributeRule();
               attributeRule.setGroup(groupedOtherAttribute.getGroup());
               attributeRule.setAttrKey(otherAttribute.getAttrKey());
               attributeRule.setAttrVals(Lists.newArrayList(new String[]{otherAttribute.getAttrVal()}));
               OtherAttributeWithRule otherAttributeWithRule = new OtherAttributeWithRule();
               otherAttributeWithRule.setAttrVal(otherAttribute.getAttrVal());
               otherAttributeWithRule.setAttributeRule(attributeRule);
               otherAttributeRules.add(attributeRule);
            }
         }

         return otherAttributeRules;
      }
   }
}
