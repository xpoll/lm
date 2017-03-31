package cn.blmdz.boot.datasource.autoconfigure.metadata;

import cn.blmdz.boot.datasource.autoconfigure.metadata.AbstractDataSourcePoolMetadata;

import com.zaxxer.hikari.HikariDataSource;
import com.zaxxer.hikari.pool.HikariPool;

import org.springframework.beans.DirectFieldAccessor;

public class HikariDataSourcePoolMetadata extends AbstractDataSourcePoolMetadata {
   public HikariDataSourcePoolMetadata(HikariDataSource dataSource) {
      super(dataSource);
   }

   public Integer getActive() {
      try {
         return Integer.valueOf(this.getHikariPool().getActiveConnections());
      } catch (Exception var2) {
         return null;
      }
   }

   private HikariPool getHikariPool() {
      return (HikariPool)(new DirectFieldAccessor(this.getDataSource())).getPropertyValue("pool");
   }

   public Integer getMax() {
      return Integer.valueOf(((HikariDataSource)this.getDataSource()).getMaximumPoolSize());
   }

   public Integer getMin() {
      return Integer.valueOf(((HikariDataSource)this.getDataSource()).getMinimumIdle());
   }

   public String getValidationQuery() {
      return ((HikariDataSource)this.getDataSource()).getConnectionTestQuery();
   }
}
