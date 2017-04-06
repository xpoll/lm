package cn.blmdz.wolf.parana.component.dto.attribute;

import java.io.Serializable;

import cn.blmdz.wolf.parana.component.dto.attribute.AttributeRule;

public class SkuAttributeKeyWithRule implements Serializable {
   private static final long serialVersionUID = -2102616773029186697L;
   private String attrKey;
   private Integer status;
   private AttributeRule attributeRule;

   public String getAttrKey() {
      return this.attrKey;
   }

   public void setAttrKey(String attrKey) {
      this.attrKey = attrKey;
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
