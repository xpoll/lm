package cn.blmdz.wolf.parana.spu.model;

import java.io.Serializable;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.springframework.util.CollectionUtils;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Strings;

import cn.blmdz.home.common.util.JsonMapper;

public class SpuAttribute implements Serializable {
   private static final long serialVersionUID = 5192201046613468045L;
   private static final ObjectMapper objectMapper = JsonMapper.nonEmptyMapper().getMapper();
   private Long spuId;
   private List skuAttrs;
   private List otherAttrs;
   @JsonIgnore
   private String skuAttrsJson;
   @JsonIgnore
   private String otherAttrsJson;
   private Date createdAt;
   private Date updatedAt;

   public void setSkuAttrs(List skuAttrs) {
      this.skuAttrs = skuAttrs;
      if(CollectionUtils.isEmpty(skuAttrs)) {
         this.skuAttrsJson = null;
      } else {
         try {
            this.skuAttrsJson = objectMapper.writeValueAsString(skuAttrs);
         } catch (Exception var3) {
            ;
         }
      }

   }

   public void setOtherAttrs(List otherAttrs) {
      this.otherAttrs = otherAttrs;
      if(otherAttrs == null) {
         this.otherAttrsJson = null;
      } else {
         try {
            this.otherAttrsJson = objectMapper.writeValueAsString(otherAttrs);
         } catch (Exception var3) {
            ;
         }
      }

   }

   public void setSkuAttrsJson(String skuAttrsJson) throws Exception {
      this.skuAttrsJson = skuAttrsJson;
      if(Strings.isNullOrEmpty(skuAttrsJson)) {
         this.skuAttrs = Collections.emptyList();
      } else {
         this.skuAttrs = (List)objectMapper.readValue(skuAttrsJson, new TypeReference() {
         });
      }

   }

   public void setOtherAttrsJson(String otherAttrsJson) throws Exception {
      this.otherAttrsJson = otherAttrsJson;
      if(Strings.isNullOrEmpty(otherAttrsJson)) {
         this.otherAttrs = Collections.emptyList();
      } else {
         this.otherAttrs = (List)objectMapper.readValue(otherAttrsJson, new TypeReference() {
         });
      }

   }

   public String toString() {
      return "SpuAttribute(spuId=" + this.getSpuId() + ", skuAttrs=" + this.getSkuAttrs() + ", otherAttrs=" + this.getOtherAttrs() + ", skuAttrsJson=" + this.getSkuAttrsJson() + ", otherAttrsJson=" + this.getOtherAttrsJson() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof SpuAttribute)) {
         return false;
      } else {
         SpuAttribute other = (SpuAttribute)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$spuId = this.getSpuId();
            Object other$spuId = other.getSpuId();
            if(this$spuId == null) {
               if(other$spuId != null) {
                  return false;
               }
            } else if(!this$spuId.equals(other$spuId)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof SpuAttribute;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $spuId = this.getSpuId();
      result = result * 59 + ($spuId == null?0:$spuId.hashCode());
      return result;
   }

   public Long getSpuId() {
      return this.spuId;
   }

   public void setSpuId(Long spuId) {
      this.spuId = spuId;
   }

   public List getSkuAttrs() {
      return this.skuAttrs;
   }

   public List getOtherAttrs() {
      return this.otherAttrs;
   }

   public String getSkuAttrsJson() {
      return this.skuAttrsJson;
   }

   public String getOtherAttrsJson() {
      return this.otherAttrsJson;
   }

   public Date getCreatedAt() {
      return this.createdAt;
   }

   public void setCreatedAt(Date createdAt) {
      this.createdAt = createdAt;
   }

   public Date getUpdatedAt() {
      return this.updatedAt;
   }

   public void setUpdatedAt(Date updatedAt) {
      this.updatedAt = updatedAt;
   }
}
