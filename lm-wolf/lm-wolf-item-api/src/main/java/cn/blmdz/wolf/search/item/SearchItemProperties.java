package cn.blmdz.wolf.search.item;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(
   prefix = "item.search"
)
public class SearchItemProperties {
   private String indexName;
   private String indexType;
   private String mappingPath;
   private Integer fullDumpRange = Integer.valueOf(3);
   private Integer batchSize = Integer.valueOf(100);

   public String getIndexName() {
      return this.indexName;
   }

   public String getIndexType() {
      return this.indexType;
   }

   public String getMappingPath() {
      return this.mappingPath;
   }

   public Integer getFullDumpRange() {
      return this.fullDumpRange;
   }

   public Integer getBatchSize() {
      return this.batchSize;
   }

   public void setIndexName(String indexName) {
      this.indexName = indexName;
   }

   public void setIndexType(String indexType) {
      this.indexType = indexType;
   }

   public void setMappingPath(String mappingPath) {
      this.mappingPath = mappingPath;
   }

   public void setFullDumpRange(Integer fullDumpRange) {
      this.fullDumpRange = fullDumpRange;
   }

   public void setBatchSize(Integer batchSize) {
      this.batchSize = batchSize;
   }
}
