package io.terminus.parana.component.dto.attribute;

import com.google.common.collect.Maps;
import io.terminus.parana.attribute.dto.AttributeMetaKey;
import java.io.Serializable;
import java.util.List;
import java.util.Map;
import org.springframework.util.CollectionUtils;

public class AttributeRule implements Serializable {
   private static final long serialVersionUID = 2329465697628574196L;
   private String attrKey;
   private String group;
   private Map attrMetas;
   private Map attrMetasForK;
   private List attrVals;

   public void setAttrMetas(Map attrMetas) {
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

   public List getAttrVals() {
      return this.attrVals;
   }

   public void setAttrVals(List attrVals) {
      this.attrVals = attrVals;
   }
}
