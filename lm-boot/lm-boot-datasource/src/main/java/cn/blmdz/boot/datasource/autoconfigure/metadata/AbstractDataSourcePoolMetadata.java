package cn.blmdz.boot.datasource.autoconfigure.metadata;

import javax.sql.DataSource;

import cn.blmdz.boot.datasource.autoconfigure.metadata.DataSourcePoolMetadata;

public abstract class AbstractDataSourcePoolMetadata implements DataSourcePoolMetadata {
   private final DataSource dataSource;

   protected AbstractDataSourcePoolMetadata(DataSource dataSource) {
      this.dataSource = dataSource;
   }

   public Float getUsage() {
      Integer maxSize = this.getMax();
      Integer currentSize = this.getActive();
      return maxSize != null && currentSize != null?(maxSize.intValue() < 0?Float.valueOf(-1.0F):(currentSize.intValue() == 0?Float.valueOf(0.0F):Float.valueOf((float)currentSize.intValue() / (float)maxSize.intValue()))):null;
   }

   protected final DataSource getDataSource() {
      return this.dataSource;
   }
}
