package cn.blmdz.wolf.item.model;

import java.io.Serializable;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Strings;

import cn.blmdz.home.common.model.Indexable;
import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.wolf.attribute.dto.SkuAttribute;
import cn.blmdz.wolf.common.constants.JacksonType;
import cn.blmdz.wolf.rule.dto.GeneralSku;

public class Sku implements GeneralSku, Serializable, Indexable {
   private static final long serialVersionUID = -2296705898220315438L;
   private static final ObjectMapper objectMapper = JsonMapper.nonEmptyMapper().getMapper();
   private Long id;
   private String skuCode;
   private Long itemId;
   private Long shopId;
   private Integer status;
   private String specification;
   private String outerSkuId;
   private String outerShopId;
   private String image;
   private String thumbnail;
   private String name;
   private String extraPriceJson;
   private Map<String, Integer> extraPrice;
   private Integer price;
   private List<SkuAttribute> attrs;
   @JsonIgnore
   private String attrsJson;
   private Integer stockType;
   private Integer stockQuantity;
   private Map<String, String> extra;
   @JsonIgnore
   private String extraJson;
   private Date createdAt;
   private Date updatedAt;

   public void setAttrs(List attrs) {
      this.attrs = attrs;
      if(attrs == null) {
         this.attrsJson = null;
      } else {
         try {
            this.attrsJson = objectMapper.writeValueAsString(attrs);
         } catch (Exception var3) {
            ;
         }
      }

   }

   public void setAttrsJson(String attrsJson) throws Exception {
      this.attrsJson = attrsJson;
      if(Strings.isNullOrEmpty(attrsJson)) {
         this.attrs = Collections.emptyList();
      } else {
         this.attrs = (List)objectMapper.readValue(attrsJson, new TypeReference() {
         });
      }

   }

   public void setExtraPriceJson(String extraPriceJson) throws Exception {
      this.extraPriceJson = extraPriceJson;
      if(Strings.isNullOrEmpty(extraPriceJson)) {
         this.extraPrice = Collections.emptyMap();
      } else {
         this.extraPrice = (Map)objectMapper.readValue(extraPriceJson, JacksonType.MAP_OF_INTEGER);
      }

   }

   public void setExtraPrice(Map extraPrice) throws Exception {
      this.extraPrice = extraPrice;
      if(extraPrice == null) {
         this.extraPriceJson = null;
      } else {
         this.extraPriceJson = objectMapper.writeValueAsString(extraPrice);
      }

   }

   public void setExtraJson(String extraJson) throws Throwable {
      try {
         this.extraJson = extraJson;
         if(Strings.isNullOrEmpty(extraJson)) {
            this.extra = Collections.emptyMap();
         } else {
            this.extra = (Map)objectMapper.readValue(extraJson, JacksonType.MAP_OF_STRING);
         }

      } catch (Throwable var3) {
         throw var3;
      }
   }

   public void setExtra(Map extra) throws Throwable {
      try {
         this.extra = extra;
         if(extra != null && !extra.isEmpty()) {
            this.extraJson = objectMapper.writeValueAsString(extra);
         } else {
            this.extraJson = null;
         }

      } catch (Throwable var3) {
         throw var3;
      }
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof Sku)) {
         return false;
      } else {
         Sku other = (Sku)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$id = this.getId();
            Object other$id = other.getId();
            if(this$id == null) {
               if(other$id != null) {
                  return false;
               }
            } else if(!this$id.equals(other$id)) {
               return false;
            }

            Object this$skuCode = this.getSkuCode();
            Object other$skuCode = other.getSkuCode();
            if(this$skuCode == null) {
               if(other$skuCode != null) {
                  return false;
               }
            } else if(!this$skuCode.equals(other$skuCode)) {
               return false;
            }

            Object this$shopId = this.getShopId();
            Object other$shopId = other.getShopId();
            if(this$shopId == null) {
               if(other$shopId != null) {
                  return false;
               }
            } else if(!this$shopId.equals(other$shopId)) {
               return false;
            }

            Object this$specification = this.getSpecification();
            Object other$specification = other.getSpecification();
            if(this$specification == null) {
               if(other$specification != null) {
                  return false;
               }
            } else if(!this$specification.equals(other$specification)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof Sku;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $id = this.getId();
      result = result * 59 + ($id == null?0:$id.hashCode());
      Object $skuCode = this.getSkuCode();
      result = result * 59 + ($skuCode == null?0:$skuCode.hashCode());
      Object $shopId = this.getShopId();
      result = result * 59 + ($shopId == null?0:$shopId.hashCode());
      Object $specification = this.getSpecification();
      result = result * 59 + ($specification == null?0:$specification.hashCode());
      return result;
   }

   public String toString() {
      return "Sku(id=" + this.getId() + ", skuCode=" + this.getSkuCode() + ", itemId=" + this.getItemId() + ", shopId=" + this.getShopId() + ", status=" + this.getStatus() + ", specification=" + this.getSpecification() + ", outerSkuId=" + this.getOuterSkuId() + ", outerShopId=" + this.getOuterShopId() + ", image=" + this.getImage() + ", thumbnail=" + this.getThumbnail() + ", name=" + this.getName() + ", extraPriceJson=" + this.getExtraPriceJson() + ", extraPrice=" + this.getExtraPrice() + ", price=" + this.getPrice() + ", attrs=" + this.getAttrs() + ", attrsJson=" + this.getAttrsJson() + ", stockType=" + this.getStockType() + ", stockQuantity=" + this.getStockQuantity() + ", extra=" + this.getExtra() + ", extraJson=" + this.getExtraJson() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }

   public Long getId() {
      return this.id;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public String getSkuCode() {
      return this.skuCode;
   }

   public void setSkuCode(String skuCode) {
      this.skuCode = skuCode;
   }

   public Long getItemId() {
      return this.itemId;
   }

   public void setItemId(Long itemId) {
      this.itemId = itemId;
   }

   public Long getShopId() {
      return this.shopId;
   }

   public void setShopId(Long shopId) {
      this.shopId = shopId;
   }

   public Integer getStatus() {
      return this.status;
   }

   public void setStatus(Integer status) {
      this.status = status;
   }

   public String getSpecification() {
      return this.specification;
   }

   public void setSpecification(String specification) {
      this.specification = specification;
   }

   public String getOuterSkuId() {
      return this.outerSkuId;
   }

   public void setOuterSkuId(String outerSkuId) {
      this.outerSkuId = outerSkuId;
   }

   public String getOuterShopId() {
      return this.outerShopId;
   }

   public void setOuterShopId(String outerShopId) {
      this.outerShopId = outerShopId;
   }

   public String getImage() {
      return this.image;
   }

   public void setImage(String image) {
      this.image = image;
   }

   public String getThumbnail() {
      return this.thumbnail;
   }

   public void setThumbnail(String thumbnail) {
      this.thumbnail = thumbnail;
   }

   public String getName() {
      return this.name;
   }

   public void setName(String name) {
      this.name = name;
   }

   public String getExtraPriceJson() {
      return this.extraPriceJson;
   }

   public Map getExtraPrice() {
      return this.extraPrice;
   }

   public Integer getPrice() {
      return this.price;
   }

   public void setPrice(Integer price) {
      this.price = price;
   }

   public List<SkuAttribute> getAttrs() {
      return this.attrs;
   }

   public String getAttrsJson() {
      return this.attrsJson;
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

   public Map getExtra() {
      return this.extra;
   }

   public String getExtraJson() {
      return this.extraJson;
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
