package cn.blmdz.wolf.component.dto.attribute;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import org.springframework.util.CollectionUtils;

import com.google.common.collect.Maps;

import cn.blmdz.wolf.attribute.dto.AttributeMetaKey;

public class AttributeRule implements Serializable {
   private static final long serialVersionUID = 2329465697628574196L;
   private String attrKey;
   private String group;
   private Map<AttributeMetaKey, String> attrMetas;
   private Map<String, String> attrMetasForK;
   private List<String> attrVals;

   public void setAttrMetas(Map<AttributeMetaKey, String> attrMetas) {
      this.attrMetas = attrMetas;
      if(CollectionUtils.isEmpty(attrMetas)) {
         this.attrMetas = null;
      } else {
         this.attrMetasForK = Maps.newHashMapWithExpectedSize(attrMetas.size());

         for(AttributeMetaKey attributeMetaKey : attrMetas.keySet()) {
            this.attrMetasForK.put(attributeMetaKey.name(), attrMetas.get(attributeMetaKey));
         }
      }

   }

   public String toString() {
      return "AttributeRule(attrKey=" + this.getAttrKey() + ", group=" + this.getGroup() + ", attrMetas=" + this.getAttrMetas() + ", attrMetasForK=" + this.getAttrMetasForK() + ", attrVals=" + this.getAttrVals() + ")";
   }

   public String getAttrKey() {
      return this.attrKey;
   }

   public void setAttrKey(String attrKey) {
      this.attrKey = attrKey;
   }

   public String getGroup() {
      return this.group;
   }

   public void setGroup(String group) {
      this.group = group;
   }

   public Map getAttrMetas() {
      return this.attrMetas;
   }

   public Map getAttrMetasForK() {
      return this.attrMetasForK;
   }

   public List<String> getAttrVals() {
      return this.attrVals;
   }

   public void setAttrVals(List attrVals) {
      this.attrVals = attrVals;
   }
}
