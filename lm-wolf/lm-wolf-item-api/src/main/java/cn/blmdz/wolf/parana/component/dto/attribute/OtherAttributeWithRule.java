package cn.blmdz.wolf.parana.component.dto.attribute;

import java.io.Serializable;

import cn.blmdz.wolf.parana.component.dto.attribute.AttributeRule;

public class OtherAttributeWithRule implements Serializable {
   private static final long serialVersionUID = -4119292651046752023L;
   private String attrVal;
   private Boolean readOnlyBySeller;
   private Integer status;
   private AttributeRule attributeRule;

   public String getAttrVal() {
      return this.attrVal;
   }

   public void setAttrVal(String attrVal) {
      this.attrVal = attrVal;
   }

   public Boolean getReadOnlyBySeller() {
      return this.readOnlyBySeller;
   }

   public void setReadOnlyBySeller(Boolean readOnlyBySeller) {
      this.readOnlyBySeller = readOnlyBySeller;
   }

   public Integer getStatus() {
      return this.status;
   }

   public void setStatus(Integer status) {
      this.status = status;
   }

   public AttributeRule getAttributeRule() {
      return this.attributeRule;
   }

   public void setAttributeRule(AttributeRule attributeRule) {
      this.attributeRule = attributeRule;
   }
}
