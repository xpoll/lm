package cn.blmdz.wolf.attribute.dto;

import com.google.common.base.Objects;

public enum PreservedGroup {
   DEFAULT,
   SPU,
   SKU,
   USER_DEFINED,
   BASIC,
   POLICY,
   SERVICE;

   public static boolean contains(String group) {
      for(PreservedGroup preservedGroup : values()) {
         if(Objects.equal(preservedGroup.name(), group)) {
            return true;
         }
      }

      return false;
   }
}
