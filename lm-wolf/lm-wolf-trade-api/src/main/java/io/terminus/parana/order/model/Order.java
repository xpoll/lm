package io.terminus.parana.order.model;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Strings;
import io.terminus.common.utils.JsonMapper;
import io.terminus.parana.common.constants.JacksonType;
import java.io.Serializable;
import java.util.Collections;
import java.util.Date;
import java.util.Map;

public class Order implements Serializable {
   private static final long serialVersionUID = 9051314216663836318L;
   private static final ObjectMapper objectMapper = JsonMapper.nonEmptyMapper().getMapper();
   private Long id;
   private Long parentId;
   private Integer parentType;
   private Long flowId;
   private Long nodeInstanceId;
   private String nextActionInstanceIds;
   private Integer type;
   private String outId;
   private String outFrom;
   private String extraJson;
   private Map extra;
   private String tagsJson;
   private Map tags;
   private Date createdAt;
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

   public void setTagsJson(String tagsJson) throws Exception {
      this.tagsJson = tagsJson;
      if(Strings.isNullOrEmpty(tagsJson)) {
         this.tags = Collections.emptyMap();
      } else {
         this.tags = (Map)objectMapper.readValue(tagsJson, JacksonType.MAP_OF_STRING);
      }

   }

   public void setTags(Map tags) {
      this.tags = tags;
      if(tags != null && !tags.isEmpty()) {
         try {
            this.tagsJson = objectMapper.writeValueAsString(tags);
         } catch (Exception var3) {
            ;
         }
      } else {
         this.tagsJson = null;
      }

   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof Order)) {
         return false;
      } else {
         Order other = (Order)o;
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

            Object this$parentId = this.getParentId();
            Object other$parentId = other.getParentId();
            if(this$parentId == null) {
               if(other$parentId != null) {
                  return false;
               }
            } else if(!this$parentId.equals(other$parentId)) {
               return false;
            }

            Object this$parentType = this.getParentType();
            Object other$parentType = other.getParentType();
            if(this$parentType == null) {
               if(other$parentType != null) {
                  return false;
               }
            } else if(!this$parentType.equals(other$parentType)) {
               return false;
            }

            Object this$flowId = this.getFlowId();
            Object other$flowId = other.getFlowId();
            if(this$flowId == null) {
               if(other$flowId != null) {
                  return false;
               }
            } else if(!this$flowId.equals(other$flowId)) {
               return false;
            }

            Object this$nodeInstanceId = this.getNodeInstanceId();
            Object other$nodeInstanceId = other.getNodeInstanceId();
            if(this$nodeInstanceId == null) {
               if(other$nodeInstanceId != null) {
                  return false;
               }
            } else if(!this$nodeInstanceId.equals(other$nodeInstanceId)) {
               return false;
            }

            Object this$nextActionInstanceIds = this.getNextActionInstanceIds();
            Object other$nextActionInstanceIds = other.getNextActionInstanceIds();
            if(this$nextActionInstanceIds == null) {
               if(other$nextActionInstanceIds != null) {
                  return false;
               }
            } else if(!this$nextActionInstanceIds.equals(other$nextActionInstanceIds)) {
               return false;
            }

            Object this$type = this.getType();
            Object other$type = other.getType();
            if(this$type == null) {
               if(other$type != null) {
                  return false;
               }
            } else if(!this$type.equals(other$type)) {
               return false;
            }

            Object this$outId = this.getOutId();
            Object other$outId = other.getOutId();
            if(this$outId == null) {
               if(other$outId != null) {
                  return false;
               }
            } else if(!this$outId.equals(other$outId)) {
               return false;
            }

            Object this$outFrom = this.getOutFrom();
            Object other$outFrom = other.getOutFrom();
            if(this$outFrom == null) {
               if(other$outFrom != null) {
                  return false;
               }
            } else if(!this$outFrom.equals(other$outFrom)) {
               return false;
            }

            Object this$extraJson = this.getExtraJson();
            Object other$extraJson = other.getExtraJson();
            if(this$extraJson == null) {
               if(other$extraJson != null) {
                  return false;
               }
            } else if(!this$extraJson.equals(other$extraJson)) {
               return false;
            }

            Object this$extra = this.getExtra();
            Object other$extra = other.getExtra();
            if(this$extra == null) {
               if(other$extra != null) {
                  return false;
               }
            } else if(!this$extra.equals(other$extra)) {
               return false;
            }

            Object this$tagsJson = this.getTagsJson();
            Object other$tagsJson = other.getTagsJson();
            if(this$tagsJson == null) {
               if(other$tagsJson != null) {
                  return false;
               }
            } else if(!this$tagsJson.equals(other$tagsJson)) {
               return false;
            }

            Object this$tags = this.getTags();
            Object other$tags = other.getTags();
            if(this$tags == null) {
               if(other$tags != null) {
                  return false;
               }
            } else if(!this$tags.equals(other$tags)) {
               return false;
            }

            Object this$createdAt = this.getCreatedAt();
            Object other$createdAt = other.getCreatedAt();
            if(this$createdAt == null) {
               if(other$createdAt != null) {
                  return false;
               }
            } else if(!this$createdAt.equals(other$createdAt)) {
               return false;
            }

            Object this$updatedAt = this.getUpdatedAt();
            Object other$updatedAt = other.getUpdatedAt();
            if(this$updatedAt == null) {
               if(other$updatedAt != null) {
                  return false;
               }
            } else if(!this$updatedAt.equals(other$updatedAt)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof Order;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $id = this.getId();
      result = result * 59 + ($id == null?0:$id.hashCode());
      Object $parentId = this.getParentId();
      result = result * 59 + ($parentId == null?0:$parentId.hashCode());
      Object $parentType = this.getParentType();
      result = result * 59 + ($parentType == null?0:$parentType.hashCode());
      Object $flowId = this.getFlowId();
      result = result * 59 + ($flowId == null?0:$flowId.hashCode());
      Object $nodeInstanceId = this.getNodeInstanceId();
      result = result * 59 + ($nodeInstanceId == null?0:$nodeInstanceId.hashCode());
      Object $nextActionInstanceIds = this.getNextActionInstanceIds();
      result = result * 59 + ($nextActionInstanceIds == null?0:$nextActionInstanceIds.hashCode());
      Object $type = this.getType();
      result = result * 59 + ($type == null?0:$type.hashCode());
      Object $outId = this.getOutId();
      result = result * 59 + ($outId == null?0:$outId.hashCode());
      Object $outFrom = this.getOutFrom();
      result = result * 59 + ($outFrom == null?0:$outFrom.hashCode());
      Object $extraJson = this.getExtraJson();
      result = result * 59 + ($extraJson == null?0:$extraJson.hashCode());
      Object $extra = this.getExtra();
      result = result * 59 + ($extra == null?0:$extra.hashCode());
      Object $tagsJson = this.getTagsJson();
      result = result * 59 + ($tagsJson == null?0:$tagsJson.hashCode());
      Object $tags = this.getTags();
      result = result * 59 + ($tags == null?0:$tags.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 59 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 59 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "Order(id=" + this.getId() + ", parentId=" + this.getParentId() + ", parentType=" + this.getParentType() + ", flowId=" + this.getFlowId() + ", nodeInstanceId=" + this.getNodeInstanceId() + ", nextActionInstanceIds=" + this.getNextActionInstanceIds() + ", type=" + this.getType() + ", outId=" + this.getOutId() + ", outFrom=" + this.getOutFrom() + ", extraJson=" + this.getExtraJson() + ", extra=" + this.getExtra() + ", tagsJson=" + this.getTagsJson() + ", tags=" + this.getTags() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }

   public Long getId() {
      return this.id;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public Long getParentId() {
      return this.parentId;
   }

   public void setParentId(Long parentId) {
      this.parentId = parentId;
   }

   public Integer getParentType() {
      return this.parentType;
   }

   public void setParentType(Integer parentType) {
      this.parentType = parentType;
   }

   public Long getFlowId() {
      return this.flowId;
   }

   public void setFlowId(Long flowId) {
      this.flowId = flowId;
   }

   public Long getNodeInstanceId() {
      return this.nodeInstanceId;
   }

   public void setNodeInstanceId(Long nodeInstanceId) {
      this.nodeInstanceId = nodeInstanceId;
   }

   public String getNextActionInstanceIds() {
      return this.nextActionInstanceIds;
   }

   public void setNextActionInstanceIds(String nextActionInstanceIds) {
      this.nextActionInstanceIds = nextActionInstanceIds;
   }

   public Integer getType() {
      return this.type;
   }

   public void setType(Integer type) {
      this.type = type;
   }

   public String getOutId() {
      return this.outId;
   }

   public void setOutId(String outId) {
      this.outId = outId;
   }

   public String getOutFrom() {
      return this.outFrom;
   }

   public void setOutFrom(String outFrom) {
      this.outFrom = outFrom;
   }

   public String getExtraJson() {
      return this.extraJson;
   }

   public Map getExtra() {
      return this.extra;
   }

   public String getTagsJson() {
      return this.tagsJson;
   }

   public Map getTags() {
      return this.tags;
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
