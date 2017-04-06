package cn.blmdz.wolf.cart.model;

import java.io.IOException;
import java.io.Serializable;
import java.util.Collections;
import java.util.Date;
import java.util.Map;

import org.springframework.util.CollectionUtils;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Strings;

import cn.blmdz.home.common.model.Indexable;
import cn.blmdz.home.common.util.JsonMapper;

public class CartItem implements Serializable, Indexable {
   private static final long serialVersionUID = -3873400543343400909L;
   private static final ObjectMapper objectMapper = JsonMapper.JSON_NON_EMPTY_MAPPER.getMapper();
   private static final TypeReference EXTRA_MAP = new TypeReference() {
   };
   private Long id;
   private Long buyerId;
   private Long shopId;
   private Long skuId;
   private Integer quantity;
   private Integer snapshotPrice;
   private Date createdAt;
   private Date updatedAt;
   @JsonIgnore
   private String extraJson;
   private Map extraMap;

   public void setExtraJson(String extraJson) throws IOException {
      this.extraJson = extraJson;
      if(Strings.isNullOrEmpty(extraJson)) {
         this.extraMap = Collections.emptyMap();
      } else {
         this.extraMap = (Map)objectMapper.readValue(extraJson, EXTRA_MAP);
      }

   }

   public void setExtraMap(Map extraMap) throws Exception {
      this.extraMap = extraMap;
      if(CollectionUtils.isEmpty(extraMap)) {
         this.extraJson = null;
      } else {
         this.extraJson = objectMapper.writeValueAsString(extraMap);
      }

   }

   public String toString() {
      return "CartItem(id=" + this.getId() + ", buyerId=" + this.getBuyerId() + ", shopId=" + this.getShopId() + ", skuId=" + this.getSkuId() + ", quantity=" + this.getQuantity() + ", snapshotPrice=" + this.getSnapshotPrice() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ", extraJson=" + this.getExtraJson() + ", extraMap=" + this.getExtraMap() + ")";
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof CartItem)) {
         return false;
      } else {
         CartItem other = (CartItem)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$buyerId = this.getBuyerId();
            Object other$buyerId = other.getBuyerId();
            if(this$buyerId == null) {
               if(other$buyerId != null) {
                  return false;
               }
            } else if(!this$buyerId.equals(other$buyerId)) {
               return false;
            }

            Object this$skuId = this.getSkuId();
            Object other$skuId = other.getSkuId();
            if(this$skuId == null) {
               if(other$skuId != null) {
                  return false;
               }
            } else if(!this$skuId.equals(other$skuId)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof CartItem;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $buyerId = this.getBuyerId();
      result = result * 59 + ($buyerId == null?0:$buyerId.hashCode());
      Object $skuId = this.getSkuId();
      result = result * 59 + ($skuId == null?0:$skuId.hashCode());
      return result;
   }

   public Long getId() {
      return this.id;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public Long getBuyerId() {
      return this.buyerId;
   }

   public void setBuyerId(Long buyerId) {
      this.buyerId = buyerId;
   }

   public Long getShopId() {
      return this.shopId;
   }

   public void setShopId(Long shopId) {
      this.shopId = shopId;
   }

   public Long getSkuId() {
      return this.skuId;
   }

   public void setSkuId(Long skuId) {
      this.skuId = skuId;
   }

   public Integer getQuantity() {
      return this.quantity;
   }

   public void setQuantity(Integer quantity) {
      this.quantity = quantity;
   }

   public Integer getSnapshotPrice() {
      return this.snapshotPrice;
   }

   public void setSnapshotPrice(Integer snapshotPrice) {
      this.snapshotPrice = snapshotPrice;
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

   public String getExtraJson() {
      return this.extraJson;
   }

   public Map getExtraMap() {
      return this.extraMap;
   }
}
