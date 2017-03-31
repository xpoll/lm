package cn.blmdz.boot.datasource.autoconfigure.metadata;

import org.apache.commons.dbcp.BasicDataSource;

import cn.blmdz.boot.datasource.autoconfigure.metadata.AbstractDataSourcePoolMetadata;

public class CommonsDbcpDataSourcePoolMetadata extends AbstractDataSourcePoolMetadata {
   public CommonsDbcpDataSourcePoolMetadata(BasicDataSource dataSource) {
      super(dataSource);
   }

   public Integer getActive() {
      return Integer.valueOf(((BasicDataSource)this.getDataSource()).getNumActive());
   }

   public Integer getMax() {
      return Integer.valueOf(((BasicDataSource)this.getDataSource()).getMaxActive());
   }

   public Integer getMin() {
      return Integer.valueOf(((BasicDataSource)this.getDataSource()).getMinIdle());
   }

   public String getValidationQuery() {
      return ((BasicDataSource)this.getDataSource()).getValidationQuery();
   }
}
