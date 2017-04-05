package io.terminus.parana.attribute.dto;

import com.google.common.primitives.Doubles;
import io.terminus.common.utils.Splitters;
import io.terminus.parana.common.utils.DateUtil;
import java.util.List;
import org.springframework.util.StringUtils;

public enum AttributeMetaKey {
   REQUIRED {
      public boolean validate(String attrVal, String metaValue, List context) {
         return StringUtils.hasText(metaValue) && Boolean.valueOf(metaValue).booleanValue()?StringUtils.hasText(attrVal):true;
      }
   },
   IMPORTANT {
      public boolean validate(String attrVal, String metaValue, List context) {
         return StringUtils.hasText(metaValue) && Boolean.valueOf(metaValue).booleanValue()?StringUtils.hasText(attrVal):true;
      }
   },
   VALUE_TYPE {
      public boolean validate(String attrVal, String metaValue, List context) {
         if(!StringUtils.hasText(attrVal)) {
            return true;
         } else if(!StringUtils.hasText(metaValue)) {
            return true;
         } else {
            metaValue = (String)Splitters.UNDERSCORE.splitToList(metaValue).get(0);
            byte var5 = -1;
            switch(metaValue.hashCode()) {
            case -1981034679:
               if(metaValue.equals("NUMBER")) {
                  var5 = 1;
               }
               break;
            case -1838656495:
               if(metaValue.equals("STRING")) {
                  var5 = 0;
               }
               break;
            case 2090926:
               if(metaValue.equals("DATE")) {
                  var5 = 2;
               }
            }

            switch(var5) {
            case 0:
               return true;
            case 1:
               return Doubles.tryParse(attrVal) != null;
            case 2:
               return DateUtil.isValidDate(attrVal);
            default:
               return false;
            }
         }
      }
   },
   SKU_CANDIDATE {
      public boolean validate(String attrVal, String metaValue, List context) {
         return true;
      }
   },
   USER_DEFINED {
      public boolean validate(String attrVal, String metaValue, List context) {
         return !StringUtils.hasText(attrVal)?true:(StringUtils.hasText(metaValue) && !Boolean.valueOf(metaValue).booleanValue()?context.contains(attrVal):true);
      }
   };

   private AttributeMetaKey() {
   }

   public abstract boolean validate(String var1, String var2, List var3);
}
