package cn.blmdz.wolf.attribute.dto;

import java.io.Serializable;
import java.util.List;

import com.google.common.base.Strings;

import cn.blmdz.home.common.util.Splitters;

public class AttributeValueType implements Serializable {
   private static final long serialVersionUID = -3545653259497157936L;
   private AttributeValueType.ValueType valueType;
   private String unit;

   public String toString() {
      return Strings.isNullOrEmpty(this.unit)?this.valueType.name():this.valueType.name() + "_" + this.unit;
   }

   public static AttributeValueType from(String input) {
      if(Strings.isNullOrEmpty(input)) {
         return null;
      } else {
         List<String> parts = Splitters.UNDERSCORE.splitToList(input);
         AttributeValueType attrValueType = new AttributeValueType();
         attrValueType.valueType = AttributeValueType.ValueType.valueOf((String)parts.get(0));
         if(parts.size() > 1) {
            attrValueType.unit = (String)parts.get(1);
         }

         return attrValueType;
      }
   }

   public AttributeValueType.ValueType getValueType() {
      return this.valueType;
   }

   public void setValueType(AttributeValueType.ValueType valueType) {
      this.valueType = valueType;
   }

   public String getUnit() {
      return this.unit;
   }

   public void setUnit(String unit) {
      this.unit = unit;
   }

   public static enum ValueType {
      TEXT,
      NUMBER,
      DATE;
   }
}
