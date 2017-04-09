package cn.blmdz.wolf.category.model;

import java.io.IOException;
import java.io.Serializable;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.springframework.util.CollectionUtils;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Strings;
import com.google.common.collect.Maps;

import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.wolf.attribute.dto.AttributeMetaKey;
import cn.blmdz.wolf.common.constants.JacksonType;

public class CategoryAttribute implements Serializable {
   private static final long serialVersionUID = 3413176817498583357L;
   private static final ObjectMapper objectMapper = JsonMapper.nonEmptyMapper().getMapper();
   private Long id;
   private Long categoryId;
   private String attrKey;
   private String group;
   private Integer index;
   private Integer status;
   private Map<AttributeMetaKey, String> attrMetas;
   private Map<String, String> attrMetasForK;
   @JsonIgnore
   private String attrMetasJson;
   private List<String> attrVals;
   private String attrValsJson;
   private Date createdAt;
   private Date updatedAt;

   public void setAttrMetas(Map<AttributeMetaKey, String> attrMetas) throws Exception {
      this.attrMetas = attrMetas;
      if(CollectionUtils.isEmpty(attrMetas)) {
         this.attrMetasJson = null;
      } else {
         this.attrMetasJson = objectMapper.writeValueAsString(attrMetas);
         this.attrMetasForK = Maps.newHashMapWithExpectedSize(attrMetas.size());

         for(AttributeMetaKey attributeMetaKey : attrMetas.keySet()) {
            this.attrMetasForK.put(attributeMetaKey.name(), attrMetas.get(attributeMetaKey));
         }
      }

   }

   public void setAttrMetasJson(String attrMetasJson) throws IOException {
      this.attrMetasJson = attrMetasJson;
      if(Strings.isNullOrEmpty(attrMetasJson)) {
         this.attrMetas = Collections.emptyMap();
      } else {
         this.attrMetas = (Map)objectMapper.readValue(attrMetasJson, new TypeReference() {
         });
         this.attrMetasForK = Maps.newHashMapWithExpectedSize(this.attrMetas.size());

         for(AttributeMetaKey attributeMetaKey : this.attrMetas.keySet()) {
            this.attrMetasForK.put(attributeMetaKey.name(), this.attrMetas.get(attributeMetaKey));
         }
      }

   }

   public void setAttrVals(List attrVals) throws Exception {
      this.attrVals = attrVals;
      if(CollectionUtils.isEmpty(attrVals)) {
         this.attrValsJson = "[]";
      } else {
         this.attrValsJson = objectMapper.writeValueAsString(attrVals);
      }

   }

   public void setAttrValsJson(String attrValsJson) throws IOException {
      this.attrValsJson = attrValsJson;
      if(Strings.isNullOrEmpty(attrValsJson)) {
         this.attrVals = Collections.emptyList();
      } else {
         this.attrVals = (List)objectMapper.readValue(attrValsJson, JacksonType.LIST_OF_STRING);
      }

   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof CategoryAttribute)) {
         return false;
      } else {
         CategoryAttribute other = (CategoryAttribute)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$categoryId = this.getCategoryId();
            Object other$categoryId = other.getCategoryId();
            if(this$categoryId == null) {
               if(other$categoryId != null) {
                  return false;
               }
            } else if(!this$categoryId.equals(other$categoryId)) {
               return false;
            }

            Object this$attrKey = this.getAttrKey();
            Object other$attrKey = other.getAttrKey();
            if(this$attrKey == null) {
               if(other$attrKey != null) {
                  return false;
               }
            } else if(!this$attrKey.equals(other$attrKey)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof CategoryAttribute;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $categoryId = this.getCategoryId();
      result = result * 59 + ($categoryId == null?0:$categoryId.hashCode());
      Object $attrKey = this.getAttrKey();
      result = result * 59 + ($attrKey == null?0:$attrKey.hashCode());
      return result;
   }

   public String toString() {
      return "CategoryAttribute(id=" + this.getId() + ", categoryId=" + this.getCategoryId() + ", attrKey=" + this.getAttrKey() + ", group=" + this.getGroup() + ")";
   }

   public Long getId() {
      return this.id;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public Long getCategoryId() {
      return this.categoryId;
   }

   public void setCategoryId(Long categoryId) {
      this.categoryId = categoryId;
   }

   public String getAttrKey() {
      return this.attrKey;
   }

   public void setAttrKey(String attrKey) {
      this.attrKey = attrKey;
   }

   public String getGroup() {
      return this.group;
   }

   public void setGroup(String group) {
      this.group = group;
   }

   public Integer getIndex() {
      return this.index;
   }

   public void setIndex(Integer index) {
      this.index = index;
   }

   public Integer getStatus() {
      return this.status;
   }

   public void setStatus(Integer status) {
      this.status = status;
   }

   public Map getAttrMetas() {
      return this.attrMetas;
   }

   public Map getAttrMetasForK() {
      return this.attrMetasForK;
   }

   public String getAttrMetasJson() {
      return this.attrMetasJson;
   }

   public List getAttrVals() {
      return this.attrVals;
   }

   public String getAttrValsJson() {
      return this.attrValsJson;
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
