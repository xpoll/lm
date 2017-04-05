package io.terminus.parana.spu.model;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Strings;
import io.terminus.common.utils.JsonMapper;
import io.terminus.parana.common.constants.JacksonType;
import java.io.Serializable;
import java.util.Collections;
import java.util.Date;
import java.util.Map;

public class Spu implements Serializable {
   private static final long serialVersionUID = 7338371379340255740L;
   private static final ObjectMapper objectMapper = JsonMapper.nonEmptyMapper().getMapper();
   private Long id;
   private String spuCode;
   private Long categoryId;
   private String name;
   private Long brandId;
   private String brandName;
   private String mainImage;
   private Integer lowPrice;
   private Integer highPrice;
   private Integer stockType;
   private Integer stockQuantity;
   private Integer status;
   private String advertise;
   private String specification;
   private Integer type;
   private Integer reduceStockType;
   @JsonIgnore
   private String extraJson;
   private Map extra;
   private String spuInfoMd5;
   @JsonIgnore
   private Date createdAt;
   @JsonIgnore
   private Date updatedAt;

   public void setExtraJson(String extraJson) throws Exception {
      this.extraJson = extraJson;
      if(Strings.isNullOrEmpty(extraJson)) {
         this.extra = Collections.emptyMap();
      } else {
         this.extra = (Map)objectMapper.readValue(extraJson, JacksonType.MAP_OF_STRING);
      }

   }

   public void setExtra(Map extra) {
      this.extra = extra;
      if(extra != null && !extra.isEmpty()) {
         try {
            this.extraJson = objectMapper.writeValueAsString(extra);
         } catch (Exception var3) {
            ;
         }
      } else {
         this.extraJson = null;
      }

   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof Spu)) {
         return false;
      } else {
         Spu other = (Spu)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$spuCode = this.getSpuCode();
            Object other$spuCode = other.getSpuCode();
            if(this$spuCode == null) {
               if(other$spuCode != null) {
                  return false;
               }
            } else if(!this$spuCode.equals(other$spuCode)) {
               return false;
            }

            Object this$categoryId = this.getCategoryId();
            Object other$categoryId = other.getCategoryId();
            if(this$categoryId == null) {
               if(other$categoryId != null) {
                  return false;
               }
            } else if(!this$categoryId.equals(other$categoryId)) {
               return false;
            }

            Object this$name = this.getName();
            Object other$name = other.getName();
            if(this$name == null) {
               if(other$name != null) {
                  return false;
               }
            } else if(!this$name.equals(other$name)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof Spu;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $spuCode = this.getSpuCode();
      result = result * 59 + ($spuCode == null?0:$spuCode.hashCode());
      Object $categoryId = this.getCategoryId();
      result = result * 59 + ($categoryId == null?0:$categoryId.hashCode());
      Object $name = this.getName();
      result = result * 59 + ($name == null?0:$name.hashCode());
      return result;
   }

   public String toString() {
      return "Spu(id=" + this.getId() + ", spuCode=" + this.getSpuCode() + ", categoryId=" + this.getCategoryId() + ", name=" + this.getName() + ", brandId=" + this.getBrandId() + ", brandName=" + this.getBrandName() + ", mainImage=" + this.getMainImage() + ", lowPrice=" + this.getLowPrice() + ", highPrice=" + this.getHighPrice() + ", stockType=" + this.getStockType() + ", stockQuantity=" + this.getStockQuantity() + ", status=" + this.getStatus() + ", advertise=" + this.getAdvertise() + ", specification=" + this.getSpecification() + ", type=" + this.getType() + ", reduceStockType=" + this.getReduceStockType() + ", extraJson=" + this.getExtraJson() + ", extra=" + this.getExtra() + ", spuInfoMd5=" + this.getSpuInfoMd5() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }

   public Long getId() {
      return this.id;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public String getSpuCode() {
      return this.spuCode;
   }

   public void setSpuCode(String spuCode) {
      this.spuCode = spuCode;
   }

   public Long getCategoryId() {
      return this.categoryId;
   }

   public void setCategoryId(Long categoryId) {
      this.categoryId = categoryId;
   }

   public String getName() {
      return this.name;
   }

   public void setName(String name) {
      this.name = name;
   }

   public Long getBrandId() {
      return this.brandId;
   }

   public void setBrandId(Long brandId) {
      this.brandId = brandId;
   }

   public String getBrandName() {
      return this.brandName;
   }

   public void setBrandName(String brandName) {
      this.brandName = brandName;
   }

   public String getMainImage() {
      return this.mainImage;
   }

   public void setMainImage(String mainImage) {
      this.mainImage = mainImage;
   }

   public Integer getLowPrice() {
      return this.lowPrice;
   }

   public void setLowPrice(Integer lowPrice) {
      this.lowPrice = lowPrice;
   }

   public Integer getHighPrice() {
      return this.highPrice;
   }

   public void setHighPrice(Integer highPrice) {
      this.highPrice = highPrice;
   }

   public Integer getStockType() {
      return this.stockType;
   }

   public void setStockType(Integer stockType) {
      this.stockType = stockType;
   }

   public Integer getStockQuantity() {
      return this.stockQuantity;
   }

   public void setStockQuantity(Integer stockQuantity) {
      this.stockQuantity = stockQuantity;
   }

   public Integer getStatus() {
      return this.status;
   }

   public void setStatus(Integer status) {
      this.status = status;
   }

   public String getAdvertise() {
      return this.advertise;
   }

   public void setAdvertise(String advertise) {
      this.advertise = advertise;
   }

   public String getSpecification() {
      return this.specification;
   }

   public void setSpecification(String specification) {
      this.specification = specification;
   }

   public Integer getType() {
      return this.type;
   }

   public void setType(Integer type) {
      this.type = type;
   }

   public Integer getReduceStockType() {
      return this.reduceStockType;
   }

   public void setReduceStockType(Integer reduceStockType) {
      this.reduceStockType = reduceStockType;
   }

   public String getExtraJson() {
      return this.extraJson;
   }

   public Map getExtra() {
      return this.extra;
   }

   public String getSpuInfoMd5() {
      return this.spuInfoMd5;
   }

   public void setSpuInfoMd5(String spuInfoMd5) {
      this.spuInfoMd5 = spuInfoMd5;
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
