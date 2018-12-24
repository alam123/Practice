using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;
using System.Windows.Threading;

namespace WpfApp1
{
    /// <summary>
    /// Interaction logic for WindowDependencyProperty.xaml
    /// </summary>
    public partial class WindowDependencyProperty : Window
    {
        public WindowDependencyProperty()
        {
            InitializeComponent();
            //this.DataContext = this;

            DispatcherTimer timer = new DispatcherTimer(TimeSpan.FromSeconds(0.5), DispatcherPriority.Normal, delegate
            {
                int newValue = 0;
                if (Counter == int.MaxValue)
                {
                    newValue = 0;
                }
                else
                {
                    newValue = Counter + 1;
                }
                SetValue(CounterProperty, newValue);
            },
            Dispatcher);
        }

        public static readonly DependencyProperty CounterProperty =
            DependencyProperty.Register("Counter", typeof(int), typeof(WindowDependencyProperty), new PropertyMetadata(0));

        public int Counter
        {
            get
            {
                return (int)GetValue(CounterProperty);
            }
            set
            {
                SetValue(CounterProperty, value);
            }
        }
    }
}