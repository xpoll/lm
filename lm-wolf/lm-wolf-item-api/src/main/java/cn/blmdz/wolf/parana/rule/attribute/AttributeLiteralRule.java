package cn.blmdz.wolf.parana.rule.attribute;

import com.google.common.base.CharMatcher;
import com.google.common.collect.Lists;

import cn.blmdz.wolf.parana.attribute.dto.GroupedOtherAttribute;
import cn.blmdz.wolf.parana.attribute.dto.GroupedSkuAttribute;
import cn.blmdz.wolf.parana.attribute.dto.OtherAttribute;
import cn.blmdz.wolf.parana.attribute.dto.SkuAttribute;
import cn.blmdz.wolf.parana.rule.RuleExecutor;
import cn.blmdz.wolf.parana.rule.dto.BaseInput;
import cn.blmdz.wolf.parana.rule.dto.BaseOutput;
import cn.blmdz.wolf.parana.rule.exception.InvalidException;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;

public class AttributeLiteralRule extends RuleExecutor {
   private static final Logger log = LoggerFactory.getLogger(AttributeLiteralRule.class);
   private final CharMatcher COLON_OR_UNDERLINE = CharMatcher.anyOf(":_");

   public void doHandleInboundData(BaseInput input, BaseOutput output) throws InvalidException {
      List<GroupedOtherAttribute> goas = input.getGroupedOtherAttributes();
      if(!CollectionUtils.isEmpty(goas)) {
         label30:
         for(GroupedOtherAttribute goa : goas) {
            List<OtherAttribute> otherAttributes = goa.getOtherAttributes();
            if(!CollectionUtils.isEmpty(otherAttributes)) {
               Iterator i$ = otherAttributes.iterator();

               String attrVal;
               while(true) {
                  if(!i$.hasNext()) {
                     continue label30;
                  }

                  OtherAttribute otherAttribute = (OtherAttribute)i$.next();
                  String attrKey = otherAttribute.getAttrKey();
                  if(this.COLON_OR_UNDERLINE.matchesAnyOf(attrKey)) {
                     log.error("attrKey:{} contains illegal characters(: or _)", attrKey);
                     throw new InvalidException("attrKey({0}).contains.colon.or.underline", new Object[]{attrKey});
                  }

                  attrVal = otherAttribute.getAttrVal();
                  if(this.COLON_OR_UNDERLINE.matchesAnyOf(attrVal)) {
                     break;
                  }
               }

               log.error("attrVal:{} contains illegal characters(: or _)", attrVal);
               throw new InvalidException("attrVal({0}).contains.colon.or.underline", new Object[]{attrVal});
            }
         }
      }

      List<GroupedSkuAttribute> gsas = input.getGroupedSkuAttributes();
      if(!CollectionUtils.isEmpty(gsas)) {
         label227:
         for(GroupedSkuAttribute gsa : gsas) {
            List<SkuAttribute> skuAttributes = gsa.getSkuAttributes();
            if(!CollectionUtils.isEmpty(skuAttributes)) {
               Iterator i$ = skuAttributes.iterator();

               String attrVal;
               while(true) {
                  if(!i$.hasNext()) {
                     continue label227;
                  }

                  SkuAttribute skuAttribute = (SkuAttribute)i$.next();
                  String attrKey = skuAttribute.getAttrKey();
                  if(this.COLON_OR_UNDERLINE.matchesAnyOf(attrKey)) {
                     log.error("attrKey:{} contains illegal characters(: or _)", attrKey);
                     throw new InvalidException("attrKey({0}).contains.colon.or.underline", new Object[]{attrKey});
                  }

                  attrVal = skuAttribute.getAttrVal();
                  if(this.COLON_OR_UNDERLINE.matchesAnyOf(attrVal)) {
                     break;
                  }
               }

               log.error("attrVal:{} contains illegal characters(: or _)", attrVal);
               throw new InvalidException("attrVal({0}).contains.colon.or.underline", new Object[]{attrVal});
            }
         }
      }

   }

   public void doHandleOutboundData(BaseInput input, BaseOutput output) {
      List<GroupedOtherAttribute> goas = input.getGroupedOtherAttributes();
      if(!CollectionUtils.isEmpty(goas)) {
         for(GroupedOtherAttribute goa : goas) {
            List<OtherAttribute> otherAttributes = goa.getOtherAttributes();
            if(!CollectionUtils.isEmpty(otherAttributes)) {
               List<OtherAttribute> validOtherAttributes = Lists.newArrayListWithCapacity(otherAttributes.size());

               for(OtherAttribute otherAttribute : otherAttributes) {
                  String attrKey = otherAttribute.getAttrKey();
                  if(this.COLON_OR_UNDERLINE.matchesAnyOf(attrKey)) {
                     log.error("attrKey:{} contains illegal characters(: or _)", attrKey);
                  } else {
                     String attrVal = otherAttribute.getAttrVal();
                     if(this.COLON_OR_UNDERLINE.matchesAnyOf(attrVal)) {
                        log.error("attrVal:{} contains illegal characters(: or _)", attrVal);
                     } else {
                        validOtherAttributes.add(otherAttribute);
                     }
                  }
               }

               goa.setOtherAttributes(validOtherAttributes);
            }
         }
      }

      List<GroupedSkuAttribute> gsas = input.getGroupedSkuAttributes();
      if(!CollectionUtils.isEmpty(gsas)) {
         for(GroupedSkuAttribute gsa : gsas) {
            List<SkuAttribute> skuAttributes = gsa.getSkuAttributes();
            if(!CollectionUtils.isEmpty(skuAttributes)) {
               List<SkuAttribute> validSkuAttributes = Lists.newArrayListWithCapacity(skuAttributes.size());

               for(SkuAttribute skuAttribute : skuAttributes) {
                  String attrKey = skuAttribute.getAttrKey();
                  if(this.COLON_OR_UNDERLINE.matchesAnyOf(attrKey)) {
                     log.error("attrKey:{} contains illegal characters(: or _)", attrKey);
                  } else {
                     String attrVal = skuAttribute.getAttrVal();
                     if(this.COLON_OR_UNDERLINE.matchesAnyOf(attrVal)) {
                        log.error("attrVal:{} contains illegal characters(: or _)", attrVal);
                     } else {
                        validSkuAttributes.add(skuAttribute);
                     }
                  }
               }

               gsa.setSkuAttributes(validSkuAttributes);
            }
         }
      }

   }

   public boolean support(BaseInput input) {
      return true;
   }

   protected List getRules(BaseInput data) {
      return Collections.emptyList();
   }
}
