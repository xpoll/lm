package cn.blmdz.boot.datasource.autoconfigure;

import java.sql.Connection;
import java.sql.SQLException;
import javax.sql.DataSource;
import org.springframework.dao.DataAccessException;
import org.springframework.jdbc.core.ConnectionCallback;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseType;
import org.springframework.util.Assert;
import org.springframework.util.ClassUtils;

public enum EmbeddedDatabaseConnection {
   NONE((EmbeddedDatabaseType)null, (String)null, (String)null),
   H2(EmbeddedDatabaseType.H2, "org.h2.Driver", "jdbc:h2:mem:%s;DB_CLOSE_DELAY=-1;DB_CLOSE_ON_EXIT=FALSE"),
   DERBY(EmbeddedDatabaseType.DERBY, "org.apache.derby.jdbc.EmbeddedDriver", "jdbc:derby:memory:%s;create=true"),
   HSQL(EmbeddedDatabaseType.HSQL, "org.hsqldb.jdbcDriver", "jdbc:hsqldb:mem:%s");

   private static final String DEFAULT_DATABASE_NAME = "testdb";
   private final EmbeddedDatabaseType type;
   private final String driverClass;
   private final String url;
   static EmbeddedDatabaseConnection override;

   private EmbeddedDatabaseConnection(EmbeddedDatabaseType type, String driverClass, String url) {
      this.type = type;
      this.driverClass = driverClass;
      this.url = url;
   }

   public String getDriverClassName() {
      return this.driverClass;
   }

   public EmbeddedDatabaseType getType() {
      return this.type;
   }

   public String getUrl() {
      return this.getUrl("testdb");
   }

   public String getUrl(String databaseName) {
      Assert.hasText(databaseName, "DatabaseName must not be null.");
      return String.format(this.url, new Object[]{databaseName});
   }

   public static boolean isEmbedded(String driverClass) {
      return driverClass != null && (driverClass.equals(HSQL.driverClass) || driverClass.equals(H2.driverClass) || driverClass.equals(DERBY.driverClass));
   }

   public static boolean isEmbedded(DataSource dataSource) {
      try {
         return ((Boolean)(new JdbcTemplate(dataSource)).execute(new EmbeddedDatabaseConnection.IsEmbedded())).booleanValue();
      } catch (DataAccessException var2) {
         return false;
      }
   }

   public static EmbeddedDatabaseConnection get(ClassLoader classLoader) {
      if(override != null) {
         return override;
      } else {
         for(EmbeddedDatabaseConnection candidate : values()) {
            if(candidate != NONE && ClassUtils.isPresent(candidate.getDriverClassName(), classLoader)) {
               return candidate;
            }
         }

         return NONE;
      }
   }

   private static class IsEmbedded implements ConnectionCallback {
      private IsEmbedded() {
      }

      public Boolean doInConnection(Connection connection) throws SQLException, DataAccessException {
         String productName = connection.getMetaData().getDatabaseProductName();
         if(productName == null) {
            return Boolean.valueOf(false);
         } else {
            productName = productName.toUpperCase();
            EmbeddedDatabaseConnection[] candidates = EmbeddedDatabaseConnection.values();

            for(EmbeddedDatabaseConnection candidate : candidates) {
               if(candidate != EmbeddedDatabaseConnection.NONE && productName.contains(candidate.name())) {
                  return Boolean.valueOf(true);
               }
            }

            return Boolean.valueOf(false);
         }
      }
   }
}
